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

@WebServlet("/login")
public class LoginServlet extends HttpServlet {

    /** 
     * Handles GET requests to show the login page.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        request.getRequestDispatcher("/login.jsp").forward(request, response);
    }

    /** 
     * Handles POST requests to process login submissions.
     * Authenticates users against the database using BCrypt password hashing.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Retrieve username and password from request
        String username = request.getParameter("username");
        String password = request.getParameter("password");

        // Get database connection and authenticate user
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Query the database for the user
            String sql = "SELECT id, password FROM users WHERE username = ?";
            PreparedStatement pstmt = conn.prepareStatement(sql);
            pstmt.setString(1, username);
            ResultSet rs = pstmt.executeQuery();
            
            // Reading user from the result set
            if (rs.next()) {

                // Extracting password and user ID
                String storedPassword = rs.getString("password");
                int userId = rs.getInt("id");
                
                // If password matches, create session and redirect to home
                if (password.equals(storedPassword)) {
                    HttpSession session = request.getSession(true);
                    session.setAttribute("username", username);
                    session.setAttribute("userId", userId);
                    session.setAttribute("loginTime", System.currentTimeMillis());
                    
                    response.sendRedirect(request.getContextPath() + "/home");
                } 
                // If password does not match, forward back to login with error
                else {
                    request.setAttribute("errorMessage", "Invalid username or password.");
                    request.getRequestDispatcher("/login.jsp").forward(request, response);
                }
            } 
            // User not found, forward back to login with error
            else {
                request.setAttribute("errorMessage", "Invalid username or password.");
                request.getRequestDispatcher("/login.jsp").forward(request, response);
            }
            
            // Clean up
            rs.close();
            pstmt.close();
            
        } 
        // Handle SQL exceptions, forward back to login with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        }
    }
}
