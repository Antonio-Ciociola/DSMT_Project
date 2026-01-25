package com.auction.controller;

import com.auction.model.User;
import com.auction.service.UserService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Optional;

@WebServlet("/login")
public class LoginServlet extends HttpServlet {

    // Service instance to handle user authentication
    private final UserService userService = new UserService();

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

        // Authenticate user
        try {

            // Use UserService to authenticate
            Optional<User> userOpt = userService.authenticate(username, password);

            // If authentication is successful, create session and redirect to home
            if (userOpt.isPresent()) {
                User user = userOpt.get();
                HttpSession session = request.getSession(true);
                session.setAttribute("username", user.getUsername());
                session.setAttribute("userId", user.getId());
                session.setAttribute("loginTime", System.currentTimeMillis());
                response.sendRedirect(request.getContextPath() + "/home");
                return;
            }

            // If authentication fails, forward back to login with error message
            request.setAttribute("errorMessage", "Invalid username or password.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        } 
        // Handle SQL exceptions during authentication
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        }
    }
}
