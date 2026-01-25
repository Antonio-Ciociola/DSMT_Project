package com.auction.controller;

import com.auction.service.UserService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.SQLException;

/**
 * Servlet implementation class SignupServlet
 * It handles user signup functionality.
 */
@WebServlet("/signup")
public class SignupServlet extends HttpServlet {

    // Service instance to handle user registration
    private final UserService userService = new UserService();

    /**
     * Handles GET requests to show the signup page.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Forward to signup.jsp
        request.getRequestDispatcher("/signup.jsp").forward(request, response);
    }

    /**
     * Handles POST requests to process signup submissions.
     * Creates a new user account in the database.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Retrieve form parameters
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        String confirmPassword = request.getParameter("confirmPassword");

        // Username Validation, return the request to signup.jsp with error message
        if (username == null || username.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Username is required.");
            request.getRequestDispatcher("/signup.jsp").forward(request, response);
            return;
        }

        // Password Validation, return the request to signup.jsp with error message
        if (password == null || password.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Password is required.");
            request.getRequestDispatcher("/signup.jsp").forward(request, response);
            return;
        }

        // Confirm Password Validation, return the request to signup.jsp with error message
        if (!password.equals(confirmPassword)) {
            request.setAttribute("errorMessage", "Passwords do not match.");
            request.getRequestDispatcher("/signup.jsp").forward(request, response);
            return;
        }

        // Register the user and forward with success message
        try {
            userService.register(username, password);
            request.setAttribute("successMessage", "Account created successfully! Please log in.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        } 
        // Handle SQL exceptions during registration, forward with error message differentiating
        // between duplicate username and other database errors
        catch (SQLException e) {
            e.printStackTrace();
            if (e.getMessage() != null && e.getMessage().contains("Duplicate entry")) {
                request.setAttribute("errorMessage", "Username already exists.");
            } else {
                request.setAttribute("errorMessage", "Database error. Please try again later.");
            }
            request.getRequestDispatcher("/signup.jsp").forward(request, response);
        }
    }
}
