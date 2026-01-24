package com.auction.controller;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;

/**
 * Servlet implementation class HomeServlet
 * It handles requests to the home page, accessible to all users and where they
 * land after login and when accessing the root URL.
 */
@WebServlet({"", "/home"})
public class HomeServlet extends HttpServlet {

    /**
     * Handles GET requests to the home page.
     * Forwards the request to home.jsp for rendering.
     * @param request  the HttpServletRequest object (JSP will need session attributes)
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Forward to home.jsp (accessible to all users)
        request.getRequestDispatcher("/home.jsp").forward(request, response);
    }
}
