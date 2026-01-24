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
import java.sql.SQLException;

/**
 * Servlet implementation class SignupServlet
 * It handles user signup functionality.
 */
@WebServlet("/signup")
public class SignupServlet extends HttpServlet {

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

        // Get database connection and create new user
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Insert new user into database
            String sql = "INSERT INTO users (username, password, balance) VALUES (?, ?, ?)";
            PreparedStatement pstmt = conn.prepareStatement(sql);
            pstmt.setString(1, username);
            pstmt.setString(2, password);
            pstmt.setDouble(3, 0.0);  // Initial balance is 0
            pstmt.executeUpdate();
            pstmt.close();
            
            // Signup successful, redirect to login
            request.setAttribute("successMessage", "Account created successfully! Please log in.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
            
        } 
        // Handle SQL exceptions (e.g., duplicate username)
        catch (SQLException e) {
            e.printStackTrace();
            if (e.getMessage().contains("Duplicate entry")) {
                request.setAttribute("errorMessage", "Username already exists.");
            } else {
                request.setAttribute("errorMessage", "Database error. Please try again later.");
            }
            request.getRequestDispatcher("/signup.jsp").forward(request, response);
        }
    }
}
