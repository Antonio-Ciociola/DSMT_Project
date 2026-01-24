<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Delete Auctions</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }

        .container {
            max-width: 900px;
            margin: 0 auto;
            background: white;
            border-radius: 10px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.1);
            padding: 40px;
        }

        .header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 30px;
            border-bottom: 2px solid #667eea;
            padding-bottom: 20px;
        }

        h1 {
            color: #333;
            font-size: 28px;
        }

        .back-link {
            display: inline-block;
            padding: 10px 20px;
            background-color: #667eea;
            color: white;
            text-decoration: none;
            border-radius: 5px;
            transition: background-color 0.3s ease;
        }

        .back-link:hover {
            background-color: #764ba2;
        }

        .error-message {
            background-color: #f8d7da;
            color: #721c24;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
            border: 1px solid #f5c6cb;
        }

        .success-message {
            background-color: #d4edda;
            color: #155724;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
            border: 1px solid #c3e6cb;
        }

        .no-auctions {
            text-align: center;
            padding: 40px 20px;
            color: #666;
        }

        .no-auctions p {
            font-size: 16px;
            margin-bottom: 20px;
        }

        .auctions-list {
            display: flex;
            flex-direction: column;
            gap: 20px;
        }

        .auction-card {
            border: 1px solid #e0e0e0;
            border-radius: 8px;
            padding: 20px;
            background-color: #f9f9f9;
            transition: all 0.3s ease;
        }

        .auction-card:hover {
            box-shadow: 0 4px 15px rgba(102, 126, 234, 0.2);
            border-color: #667eea;
        }

        .auction-title {
            font-size: 20px;
            font-weight: bold;
            color: #333;
            margin-bottom: 10px;
        }

        .auction-details {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin: 15px 0;
        }

        .detail-item {
            display: flex;
            flex-direction: column;
        }

        .detail-label {
            font-size: 12px;
            color: #999;
            font-weight: bold;
            text-transform: uppercase;
            margin-bottom: 5px;
        }

        .detail-value {
            font-size: 14px;
            color: #333;
        }

        .auction-description {
            color: #666;
            margin: 15px 0;
            font-size: 14px;
            line-height: 1.5;
        }

        .auction-actions {
            display: flex;
            gap: 10px;
            margin-top: 15px;
        }

        .delete-form {
            display: inline;
        }

        .delete-btn {
            padding: 10px 20px;
            background-color: #dc3545;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            font-size: 14px;
            transition: background-color 0.3s ease;
        }

        .delete-btn:hover {
            background-color: #c82333;
        }

        .delete-btn:active {
            transform: scale(0.98);
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Delete Auctions</h1>
            <a href="${pageContext.request.contextPath}/home" class="back-link">← Back to Home</a>
        </div>

        <!-- Display error message if present -->
        <c:if test="${not empty errorMessage}">
            <div class="error-message">
                ${errorMessage}
            </div>
        </c:if>

        <!-- Display auctions list or no auctions message -->
        <c:choose>
            <c:when test="${empty auctions}">
                <div class="no-auctions">
                    <p>You have no auctions scheduled to start.</p>
                    <p>All your auctions have either already started or been deleted.</p>
                </div>
            </c:when>
            <c:otherwise>
                <div class="auctions-list">
                    <c:forEach var="auction" items="${auctions}">
                        <div class="auction-card">
                            <div class="auction-title">${auction.title}</div>
                            
                            <c:if test="${not empty auction.description}">
                                <div class="auction-description">
                                    ${auction.description}
                                </div>
                            </c:if>

                            <div class="auction-details">
                                <div class="detail-item">
                                    <span class="detail-label">Starting Price</span>
                                    <span class="detail-value">$${auction.starting_price}</span>
                                </div>
                                <div class="detail-item">
                                    <span class="detail-label">Min Bid Increment</span>
                                    <span class="detail-value">$${auction.min_bid_increment}</span>
                                </div>
                                <div class="detail-item">
                                    <span class="detail-label">Bid Countdown Timer</span>
                                    <span class="detail-value">${auction.countdown_timer} min</span>
                                </div>
                                <div class="detail-item">
                                    <span class="detail-label">Start Date & Time</span>
                                    <span class="detail-value">${auction.start_date}</span>
                                </div>
                            </div>

                            <div class="auction-actions">
                                <form class="delete-form" method="POST" action="${pageContext.request.contextPath}/delete-auction" onsubmit="return confirm('Are you sure you want to delete this auction?');">
                                    <input type="hidden" name="auctionId" value="${auction.id}">
                                    <button type="submit" class="delete-btn">Delete Auction</button>
                                </form>
                            </div>
                        </div>
                    </c:forEach>
                </div>
            </c:otherwise>
        </c:choose>
    </div>
</body>
</html>
