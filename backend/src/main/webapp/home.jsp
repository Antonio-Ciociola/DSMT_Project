<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%
    String username = (String) session.getAttribute("username");
    if (username == null) {
        username = "Guest";
    }
%>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Auction System - Home</title>
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
        
        .user-info {
            display: flex;
            align-items: center;
            gap: 20px;
        }
        
        .user-info span {
            font-size: 14px;
        }
        
        .logout-btn {
            background: rgba(255, 255, 255, 0.2);
            color: white;
            border: 1px solid white;
            padding: 8px 16px;
            border-radius: 5px;
            cursor: pointer;
            font-size: 14px;
            transition: background 0.3s;
            text-decoration: none;
            display: inline-block;
        }
        
        .logout-btn:hover {
            background: rgba(255, 255, 255, 0.3);
        }
        
        .container {
            max-width: 1200px;
            margin: 40px auto;
            padding: 0 20px;
        }
        
        .welcome-section {
            background: white;
            padding: 40px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
            margin-bottom: 40px;
        }
        
        .welcome-section h2 {
            color: #667eea;
            margin-bottom: 20px;
            font-size: 24px;
        }
        
        .welcome-section p {
            line-height: 1.6;
            color: #555;
        }
        
        .features {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
        }
        
        .feature-card {
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
            transition: transform 0.3s, box-shadow 0.3s;
            text-decoration: none;
            display: block;
            color: inherit;
            cursor: pointer;
        }
        
        .feature-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 20px rgba(0, 0, 0, 0.15);
            background: linear-gradient(135deg, rgba(102, 126, 234, 0.05) 0%, rgba(118, 75, 162, 0.05) 100%);
        }
        
        .feature-card h3 {
            color: #667eea;
            margin-bottom: 15px;
            font-size: 18px;
        }
        
        .feature-card p {
            color: #666;
            font-size: 14px;
            line-height: 1.6;
        }
        
        .feature-icon {
            font-size: 40px;
            margin-bottom: 15px;
        }
    </style>
</head>
<body>
    <header>
        <div class="header-content">
            <h1>🏆 Auction System</h1>
            <div class="user-info">
                <span>Welcome, <strong><%= username %></strong></span>
                <% if ("Guest".equals(username)) { %>
                    <a href="${pageContext.request.contextPath}/login.jsp" class="logout-btn">Login</a>
                <% } else { %>
                    <a href="${pageContext.request.contextPath}/logout" class="logout-btn">Logout</a>
                <% } %>
            </div>
        </div>
    </header>
    
    <div class="container">
        <div class="welcome-section">
            <h2>Welcome to the Auction System!</h2>
            <p>
                You have successfully logged in. This is your dashboard where you can browse auctions, 
                place bids, delete auctions, manage balance. Use the features below to get started.
            </p>
        </div>
        
        <div class="features">
            <a href="${pageContext.request.contextPath}/browse-auctions" class="feature-card">
                <div class="feature-icon">📦</div>
                <h3>Browse Auctions</h3>
                <p>Explore active auctions and find items of interest.</p>
            </a>
            
            <% if ("Guest".equals(username)) { %>
                <a href="${pageContext.request.contextPath}/login.jsp" class="feature-card">
            <% } else { %>
                <a href="${pageContext.request.contextPath}/create-auction" class="feature-card">
            <% } %>
                <div class="feature-icon">🏷️</div>
                <h3>Create Auction</h3>
                <p>List your own items for auction. Set starting price, duration, and detailed descriptions.</p>
            </a>
            
            <% if ("Guest".equals(username)) { %>
                <a href="${pageContext.request.contextPath}/login.jsp" class="feature-card">
            <% } else { %>
                <a href="${pageContext.request.contextPath}/delete-auction" class="feature-card">
            <% } %>
                <div class="feature-icon">🗑️</div>
                <h3>Delete Auctions</h3>
                <p>Remove your auctions that are no longer needed.</p>
            </a>
            
            <% if ("Guest".equals(username)) { %>
                <a href="${pageContext.request.contextPath}/login.jsp" class="feature-card">
            <% } else { %>
                <a href="${pageContext.request.contextPath}/balance" class="feature-card">
            <% } %>
                <div class="feature-icon">💰</div>
                <h3>Manage Balance</h3>
                <p>View your account balance, deposit funds.</p>
            </a>
        </div>
    </div>
</body>
</html>
