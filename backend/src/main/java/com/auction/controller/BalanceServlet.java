package com.auction.controller;

import com.auction.service.UserService;
import com.auction.util.ValidationUtils;
import com.auction.util.ValidationUtils.ValidationException;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.SQLException;

/**
 * Servlet for viewing and updating user balance.
 */
@WebServlet("/balance")
public class BalanceServlet extends HttpServlet {

    // Service instance to interact with user data
    private final UserService userService = new UserService();

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


        // Retrieve user balance from database and set as request attribute
        try {
            BigDecimal balance = userService.getBalance(userId);
            request.setAttribute("balance", balance.setScale(2, RoundingMode.HALF_UP));
        } 
        // Handle SQL exceptions during balance retrieval
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
        String error = ValidationUtils.validateRequired(amountStr, "Amount");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            doGet(request, response);
            return;
        }

        // Parse amount to BigDecimal and validate it, if invalid forward with error
        BigDecimal amount;
        try {
            amount = ValidationUtils.validateStrictlyPositiveBigDecimal(amountStr, "Amount");
        } catch (ValidationException e) {
            request.setAttribute("errorMessage", e.getMessage());
            doGet(request, response);
            return;
        }

        // Update balance in database and handle potential SQL exceptions
        try {
            userService.addToBalance(userId, amount);
            response.sendRedirect(request.getContextPath() + "/balance?success=1");
        } catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            doGet(request, response);
        }
    }
}
