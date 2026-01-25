package com.auction.controller;

import com.auction.model.Auction;
import com.auction.service.AuctionService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

/**
 * Servlet implementation class DeleteAuctionServlet
 * Handles deletion of user's auctions that have not yet started.
 */
@WebServlet("/delete-auction")
public class DeleteAuctionServlet extends HttpServlet {

    private final AuctionService auctionService = new AuctionService();

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

        // Fetch user's upcoming auctions from the service layer
        try {
            List<Auction> auctions = auctionService.getUserUpcomingAuctions(userId);
            request.setAttribute("auctions", auctions);
        } 
        // Handle SQL exceptions during auction retrieval
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
        }

        // Forward to the JSP page for rendering
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

        // Attempt to delete the auction and handle potential SQL exceptions
        try {
            boolean deleted = auctionService.deleteUserUpcomingAuction(auctionId, userId);
            
            // If auction not found or already started, forward with error message
            if (!deleted) {
                request.setAttribute("errorMessage", "Auction not found or already started.");
                doGet(request, response);
                return;
            }

            // Redirect to the delete auction page after successful deletion
            response.sendRedirect(request.getContextPath() + "/delete-auction");
        } 
        // Handle SQL exceptions during auction deletion
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            doGet(request, response);
        }
    }
}
