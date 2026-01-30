package com.auction.job;

import com.auction.dao.AuctionDao;
import com.auction.model.Auction;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.Optional;

// Job class to start an auction by updating its status
public class AuctionStarterJob implements Job {
    
    private static final String ERLANG_API_URL = "http://erlang-master:8081/erlangapi/auction";
    
    /**
     * Execute the job to start the auction.
     * @param context The job execution context
     * @throws JobExecutionException If there is an error during job execution
     */
    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        
        try {
            // Get the auction ID from the job data map
            int auctionId = (int) context.getJobDetail().getJobDataMap().get("auctionId");
            
            // Retrieve the full auction details
            AuctionDao auctionDao = new AuctionDao();
            Optional<Auction> auctionOpt = auctionDao.findById(auctionId);
            
            if (auctionOpt.isEmpty()) {
                throw new JobExecutionException("Auction with ID " + auctionId + " not found");
            }
            
            Auction auction = auctionOpt.get();
            
            // Update the auction status to "ongoing"
            auctionDao.updateAuctionStatus(auctionId, "ongoing");

            // Send auction details to Erlang API and get WebSocket URL
            String websocketUrl = postAuctionToErlang(auction);
            
            // Save WebSocket URL to database
            if (websocketUrl != null) {
                auctionDao.updateWebsocketUrl(String.valueOf(auctionId), websocketUrl);
                System.out.println("Saved WebSocket URL for auction " + auctionId + ": " + websocketUrl);
            }
            
            System.out.println("Auction " + auctionId + " started successfully at " + System.currentTimeMillis());
        } 
        // Handle SQL exceptions during auction start
        catch (SQLException e) {
            System.err.println("Error starting auction: " + e.getMessage());
            throw new JobExecutionException("Failed to start auction", e);
        }
        // Handle IO exceptions during API call
        catch (IOException e) {
            System.err.println("Error calling Erlang API: " + e.getMessage());
            throw new JobExecutionException("Failed to notify Erlang system", e);
        }
    }
    
    /**
     * Send auction details to the Erlang API endpoint
     * @param auction The auction to send
     * @return The WebSocket URL returned by Erlang, or null if failed
     * @throws IOException If there is an error during the HTTP request
     */
    private String postAuctionToErlang(Auction auction) throws IOException {
        URL url = new URL(ERLANG_API_URL);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        
        try {
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type", "application/json");
            
            // Add API key for authentication
            String apiKey = System.getenv().getOrDefault("ERLANG_API_KEY", "auction_secret_key_2026");
            conn.setRequestProperty("X-API-Key", apiKey);
            
            conn.setDoOutput(true);
            
            // Build JSON payload with required fields
            String jsonPayload = String.format(
                "{\"id\":\"%d\",\"starting_price\":%.2f,\"min_duration\":%d,\"min_increment_bid\":%.2f,\"time_increment_bid\":%d}",
                auction.getId(),
                auction.getStartingPrice(),
                auction.getStartingDuration(),
                auction.getMinBidIncrement(),
                auction.getBidTimeIncrement()
            );
            
            // Send the request
            try (OutputStream os = conn.getOutputStream()) {
                byte[] input = jsonPayload.getBytes(StandardCharsets.UTF_8);
                os.write(input, 0, input.length);
            }
            
            // Check response code
            int responseCode = conn.getResponseCode();
            if (responseCode == HttpURLConnection.HTTP_OK) {
                // Read response to extract WebSocket URL
                try (java.io.BufferedReader br = new java.io.BufferedReader(
                        new java.io.InputStreamReader(conn.getInputStream(), "utf-8"))) {
                    StringBuilder response = new StringBuilder();
                    String responseLine;
                    while ((responseLine = br.readLine()) != null) {
                        response.append(responseLine.trim());
                    }
                    
                    String responseStr = response.toString();
                    System.out.println("Erlang response: " + responseStr);
                    
                    // Extract websocket_url from JSON response
                    int wsUrlStart = responseStr.indexOf("\"websocket_url\":\"") + 17;
                    int wsUrlEnd = responseStr.indexOf("\"", wsUrlStart);
                    
                    if (wsUrlStart > 16 && wsUrlEnd > wsUrlStart) {
                        String websocketUrl = responseStr.substring(wsUrlStart, wsUrlEnd);
                        System.out.println("Successfully registered auction " + auction.getId() + " with WebSocket URL: " + websocketUrl);
                        return websocketUrl;
                    }
                }
            } else {
                System.err.println("Failed to register auction with Erlang system. Response code: " + responseCode);
            }
            return null;
        }catch (Exception e){
            throw new IOException("Error during HTTP request to Erlang API", e);
        } 
        finally {
            conn.disconnect();
        }
    }
}
