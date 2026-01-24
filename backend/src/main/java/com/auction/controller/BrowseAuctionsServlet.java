package com.auction.controller;

import com.auction.model.DatabaseConnection;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Servlet implementation class BrowseAuctionsServlet
 * It handles displaying all auctions to users.
 */
@WebServlet("/browse-auctions")
public class BrowseAuctionsServlet extends HttpServlet {

    /**
     * Handles GET requests to display all auctions.
     * Fetches auction data from the database and forwards to JSP for rendering.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // List to hold auction data
        List<Map<String, Object>> auctions = new ArrayList<>();
        
        // Query all auctions with user information
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Defining and ExecutingSQL query to fetch auctions and associated usernames
            String sql = "SELECT a.id, a.title, a.description, a.starting_price, a.min_bid_increment, " +
                         "a.countdown_timer, a.start_date, u.username " +
                         "FROM auctions a " +
                         "JOIN users u ON a.user_id = u.id " +
                         "WHERE a.start_date >= NOW() " +
                         "ORDER BY a.start_date ASC";
            PreparedStatement pstmt = conn.prepareStatement(sql);
            ResultSet rs = pstmt.executeQuery();
            
            // Process result set by creating auction as a map and populate auctions list
            while (rs.next()) {
                Map<String, Object> auction = new HashMap<>();
                auction.put("id", rs.getInt("id"));
                auction.put("title", rs.getString("title"));
                auction.put("description", rs.getString("description"));
                auction.put("startingPrice", rs.getDouble("starting_price"));
                auction.put("minBidIncrement", rs.getDouble("min_bid_increment"));
                auction.put("countdownTimer", rs.getInt("countdown_timer"));
                auction.put("startDate", rs.getString("start_date"));
                auction.put("username", rs.getString("username"));
                auctions.add(auction);
            }
            
            // Close resources
            rs.close();
            pstmt.close();
            
        } 
        // Handle SQL exceptions, forward back to browse auctions with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Could not fetch auctions.");
        }
        
        // Set auctions in request and forward to JSP
        request.setAttribute("auctions", auctions);
        request.getRequestDispatcher("/browse-auctions.jsp").forward(request, response);
    }
}
