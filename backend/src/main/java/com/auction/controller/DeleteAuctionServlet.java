package com.auction.controller;

import com.auction.model.DatabaseConnection;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
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
 * Servlet implementation class DeleteAuctionServlet
 * Handles deletion of user's auctions that have not yet started.
 */
@WebServlet("/delete-auction")
public class DeleteAuctionServlet extends HttpServlet {

    /**
     * Handles GET requests to show the delete auction page.
     * Displays a list of auctions created by the user that haven't started yet.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Extract userid and username from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        Integer userId = (session != null) ? (Integer) session.getAttribute("userId") : null;

        // Redirect user to login if not logged in
        if (username == null || "Guest".equals(username) || userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }

        // List of the user's auctions that haven't started yet
        List<Map<String, Object>> userAuctions = new ArrayList<>();

        // Fetch user's future auctions from the database
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Query to get user's auctions that have start_date > NOW()
            String sql = "SELECT id, title, description, starting_price, min_bid_increment, " +
                         "countdown_timer, start_date FROM auctions " +
                         "WHERE user_id = ? AND start_date > NOW() " +
                         "ORDER BY start_date ASC";

            // Preparing and executing the SQL statement
            PreparedStatement pstmt = conn.prepareStatement(sql);
            pstmt.setInt(1, userId);
            ResultSet rs = pstmt.executeQuery();

            // Processing the result set and populating the userAuctions list
            while (rs.next()) {
                Map<String, Object> auction = new HashMap<>();
                auction.put("id", rs.getInt("id"));
                auction.put("title", rs.getString("title"));
                auction.put("description", rs.getString("description"));
                auction.put("starting_price", rs.getDouble("starting_price"));
                auction.put("min_bid_increment", rs.getDouble("min_bid_increment"));
                auction.put("countdown_timer", rs.getInt("countdown_timer"));
                auction.put("start_date", rs.getString("start_date"));
                userAuctions.add(auction); 
            }

            // Closing resources
            rs.close();
            pstmt.close();

        } 
        // Handle SQL exceptions, forward back to delete auction with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
        }

        // Set auctions list as request attribute and forward to JSP
        request.setAttribute("auctions", userAuctions);
        request.getRequestDispatcher("/delete-auction.jsp").forward(request, response);
    }

    /**
     * Handles POST requests to delete an auction.
     * Only allows deletion of auctions owned by the user and that haven't started yet.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Extract username and userid from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        Integer userId = (session != null) ? (Integer) session.getAttribute("userId") : null;

        // Redirect user to login if not logged in
        if (username == null || "Guest".equals(username) || userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }

        // Get auction ID String from request and check if it's empty, forward with error if so
        String auctionIdStr = request.getParameter("auctionId");
        if (auctionIdStr == null || auctionIdStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Invalid auction ID.");
            doGet(request, response);
            return;
        }

        // Parse auction ID to integer, forward with error if invalid
        int auctionId;
        try {
            auctionId = Integer.parseInt(auctionIdStr);
        } catch (NumberFormatException e) {
            request.setAttribute("errorMessage", "Invalid auction ID.");
            doGet(request, response);
            return;
        }

        // Delete the auction (only if it belongs to the user and hasn't started)
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Query to get the auction with id of the auction to delete, the id of the 
            // user who requests the deletion and a start date in the future
            String verifySQL = "SELECT id FROM auctions WHERE id = ? AND user_id = ? AND start_date > NOW()";
            PreparedStatement verifyPstmt = conn.prepareStatement(verifySQL);
            verifyPstmt.setInt(1, auctionId);
            verifyPstmt.setInt(2, userId);
            ResultSet rs = verifyPstmt.executeQuery();

            // If no such auction exists then either the auction doesn't exist or it doesn't 
            // belong to the user or it has already started, forward with error 
            if (!rs.next()) {
                request.setAttribute("errorMessage", "Auction not found or already started.");
                rs.close();
                verifyPstmt.close();
                doGet(request, response);
                return;
            }

            // Close verification resources
            rs.close();
            verifyPstmt.close();

            // Query to delete the auction
            String deleteSQL = "DELETE FROM auctions WHERE id = ? AND user_id = ?";
            PreparedStatement deletePstmt = conn.prepareStatement(deleteSQL);
            deletePstmt.setInt(1, auctionId);
            deletePstmt.setInt(2, userId);

            // Delete the auction
            deletePstmt.executeUpdate();
            deletePstmt.close();

            // Redirect back to delete auction page with success
            response.sendRedirect(request.getContextPath() + "/delete-auction");

        } 
        // Handle SQL exceptions, forward back to delete auction with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            doGet(request, response);
        }
    }
}
