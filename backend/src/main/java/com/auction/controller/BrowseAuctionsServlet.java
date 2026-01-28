package com.auction.controller;

import com.auction.model.Auction;
import com.auction.model.User;
import com.auction.service.AuctionService;
import com.auction.service.UserService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

/**
 * Servlet implementation class BrowseAuctionsServlet
 * It handles requests to browse all auctions (ongoing, upcoming, finished).
 */
@WebServlet("/browse-auctions")
public class BrowseAuctionsServlet extends HttpServlet {

    // Service instance to interact with auction data
    private final AuctionService auctionService = new AuctionService();
    private final UserService userService = new UserService();

    /**
     * Handles GET requests to display all auctions.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Fetch all auctions from the service layer
        try {
            List<Auction> auctions = auctionService.getAllAuctions();
            request.setAttribute("auctions", auctions);
            
            // Get user's current auction_id_bidding if logged in
            HttpSession session = request.getSession(false);
            if (session != null) {
                Integer userId = (Integer) session.getAttribute("userId");
                if (userId != null) {
                    Optional<User> userOpt = userService.getUserById(userId);
                    if (userOpt.isPresent()) {
                        Integer currentAuctionId = userOpt.get().getAuctionIdBidding();
                        if (currentAuctionId != null) {
                            request.setAttribute("currentAuctionIdBidding", currentAuctionId);
                        }
                    }
                }
            }
        } 
        // Handle SQL exceptions during auction retrieval
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Could not fetch auctions.");
        }

        // Forward to the JSP page for rendering
        request.getRequestDispatcher("/browse-auctions.jsp").forward(request, response);
    }
}
