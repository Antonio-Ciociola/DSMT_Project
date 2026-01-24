<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Map" %>
<%
    @SuppressWarnings("unchecked")
    List<Map<String, Object>> auctions = (List<Map<String, Object>>) request.getAttribute("auctions");
    String errorMessage = (String) request.getAttribute("errorMessage");
%>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Auction System - Browse Auctions</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: #f5f5f5;
            color: #333;
        }
        
        header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px 0;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        
        .header-content {
            max-width: 1200px;
            margin: 0 auto;
            padding: 0 20px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        h1 {
            font-size: 28px;
        }
        
        .back-link {
            background: rgba(255, 255, 255, 0.2);
            color: white;
            padding: 8px 16px;
            border-radius: 5px;
            text-decoration: none;
            transition: background 0.3s;
        }
        
        .back-link:hover {
            background: rgba(255, 255, 255, 0.3);
        }
        
        .container {
            max-width: 1200px;
            margin: 40px auto;
            padding: 0 20px;
        }
        
        .error-message {
            background: #fee;
            border-left: 4px solid #e74c3c;
            color: #c0392b;
            padding: 12px;
            margin-bottom: 20px;
            border-radius: 4px;
            font-size: 14px;
        }
        
        .auctions-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 20px;
        }
        
        .auction-card {
            background: white;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
            overflow: hidden;
            transition: transform 0.3s, box-shadow 0.3s;
        }
        
        .auction-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 20px rgba(0, 0, 0, 0.15);
        }
        
        .auction-header {
            padding: 20px;
            border-bottom: 1px solid #eee;
        }
        
        .auction-title {
            font-size: 18px;
            font-weight: 600;
            color: #333;
            margin-bottom: 8px;
        }
        
        .auction-seller {
            font-size: 13px;
            color: #666;
        }
        
        .auction-body {
            padding: 20px;
        }
        
        .auction-description {
            font-size: 14px;
            color: #666;
            margin-bottom: 15px;
            line-height: 1.5;
            max-height: 80px;
            overflow: hidden;
            text-overflow: ellipsis;
        }
        
        .auction-details {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 15px;
            margin-bottom: 15px;
            font-size: 13px;
        }
        
        .detail-item {
            background: #f9f9f9;
            padding: 10px;
            border-radius: 5px;
        }
        
        .detail-label {
            color: #999;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 11px;
            margin-bottom: 4px;
        }
        
        .detail-value {
            font-size: 15px;
            color: #333;
            font-weight: 600;
        }
        
        .price {
            color: #667eea;
        }
        
        .auction-footer {
            padding: 15px 20px;
            border-top: 1px solid #eee;
            background: #f9f9f9;
            display: flex;
            gap: 10px;
        }
        
        .bid-btn {
            flex: 1;
            padding: 10px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border: none;
            border-radius: 5px;
            font-weight: 600;
            font-size: 14px;
            cursor: pointer;
            transition: transform 0.2s;
        }
        
        .bid-btn:hover {
            transform: scale(1.05);
        }
        
        .no-auctions {
            text-align: center;
            padding: 60px 20px;
            background: white;
            border-radius: 10px;
        }
        
        .no-auctions h2 {
            color: #999;
            margin-bottom: 10px;
        }
        
        .no-auctions p {
            color: #bbb;
        }
    </style>
</head>
<body>
    <header>
        <div class="header-content">
            <h1>📦 Browse Auctions</h1>
            <a href="${pageContext.request.contextPath}/home" class="back-link">← Back to Home</a>
        </div>
    </header>
    
    <div class="container">
        <% if (errorMessage != null) { %>
            <div class="error-message"><%= errorMessage %></div>
        <% } %>
        
        <% if (auctions != null && !auctions.isEmpty()) { %>
            <div class="auctions-grid">
                <% for (Map<String, Object> auction : auctions) { %>
                    <div class="auction-card">
                        <div class="auction-header">
                            <div class="auction-title"><%= auction.get("title") %></div>
                            <div class="auction-seller">Seller: <strong><%= auction.get("username") %></strong></div>
                        </div>
                        
                        <div class="auction-body">
                            <% if (auction.get("description") != null && !auction.get("description").toString().isEmpty()) { %>
                                <div class="auction-description"><%= auction.get("description") %></div>
                            <% } %>
                            
                            <div class="auction-details">
                                <div class="detail-item">
                                    <div class="detail-label">Starting Price</div>
                                    <div class="detail-value price">$<%= String.format("%.2f", auction.get("startingPrice")) %></div>
                                </div>
                                
                                <div class="detail-item">
                                    <div class="detail-label">Min Bid Inc.</div>
                                    <div class="detail-value">$<%= String.format("%.2f", auction.get("minBidIncrement")) %></div>
                                </div>
                                
                                <div class="detail-item">
                                    <div class="detail-label">Bid Countdown</div>
                                    <div class="detail-value"><%= auction.get("countdownTimer") %> min</div>
                                </div>
                                
                                <div class="detail-item">
                                    <div class="detail-label">Starts</div>
                                    <div class="detail-value"><%= auction.get("startDate") %></div>
                                </div>
                            </div>
                        </div>
                        
                        <div class="auction-footer">
                            <button class="bid-btn">Join</button>
                        </div>
                    </div>
                <% } %>
            </div>
        <% } else { %>
            <div class="no-auctions">
                <h2>No Auctions Available</h2>
                <p>There are currently no auctions. Check back later!</p>
            </div>
        <% } %>
    </div>
</body>
</html>
