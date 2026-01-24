package com.auction.controller;

import com.auction.model.DatabaseConnection;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Servlet for viewing and updating user balance.
 */
@WebServlet("/balance")
public class BalanceServlet extends HttpServlet {

    /**
     * Handles GET requests to show the user's balance.
     * Redirects guests to login page.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Extract username and userId from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        Integer userId = (session != null) ? (Integer) session.getAttribute("userId") : null;

        // Redirect user to login if not logged in
        if (username == null || "Guest".equals(username) || userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }

        // Optional success message after balance update
        if ("1".equals(request.getParameter("success"))) {
            request.setAttribute("successMessage", "Balance updated successfully.");
        }

        // Fetch user's balance from the database
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Query to get user's balance
            String sql = "SELECT balance FROM users WHERE id = ?";
            PreparedStatement pstmt = conn.prepareStatement(sql);
            pstmt.setInt(1, userId);
            ResultSet rs = pstmt.executeQuery();

            // If user found, set balance attribute
            if (rs.next()) {
                BigDecimal balance = rs.getBigDecimal("balance");
                if (balance == null) {
                    balance = BigDecimal.ZERO;
                }
                request.setAttribute("balance", balance.setScale(2, RoundingMode.HALF_UP));
            } 
            // If user not found, set error message
            else {
                request.setAttribute("errorMessage", "User not found.");
            }

            // Close resources
            rs.close();
            pstmt.close();
        } 
        // Handle SQL exceptions
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
        }

        // Forward to balance JSP
        request.getRequestDispatcher("/balance.jsp").forward(request, response);
    }


    /**
     * Handles POST requests to update the user's balance.
     * Validates input and updates balance in the database.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Extract username and userId from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        Integer userId = (session != null) ? (Integer) session.getAttribute("userId") : null;

        // Redirect user to login if not logged in
        if (username == null || "Guest".equals(username) || userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }

        // Get amount parameter and check if the string is valid
        String amountStr = request.getParameter("amount");
        if (amountStr == null || amountStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Amount is required.");
            doGet(request, response);
            return;
        }

        // Parse amount to BigDecimal and if invalid, forward with error
        BigDecimal amount;
        try {
            amount = new BigDecimal(amountStr.trim());
        } catch (NumberFormatException e) {
            request.setAttribute("errorMessage", "Invalid amount format.");
            doGet(request, response);
            return;
        }

        // Check if amount is positive, forward with error if not
        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
            request.setAttribute("errorMessage", "Amount must be positive.");
            doGet(request, response);
            return;
        }

        // Update user's balance in the database
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Preparation of SQL statement to update user's balance
            String sql = "UPDATE users SET balance = balance + ? WHERE id = ?";
            PreparedStatement pstmt = conn.prepareStatement(sql);
            pstmt.setBigDecimal(1, amount);
            pstmt.setInt(2, userId);

            // Execute update and close resources
            pstmt.executeUpdate();
            pstmt.close();

            // Redirect back to balance page with success message
            response.sendRedirect(request.getContextPath() + "/balance?success=1");
        } 
        // Handle SQL exceptions, forward back to balance with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            doGet(request, response);
        }
    }
}
