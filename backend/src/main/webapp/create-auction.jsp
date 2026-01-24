<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%
    String errorMessage = (String) request.getAttribute("errorMessage");
%>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Auction System - Create Auction</title>
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
            max-width: 600px;
            margin: 40px auto;
            padding: 0 20px;
        }
        
        .form-card {
            background: white;
            padding: 40px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        
        h2 {
            color: #667eea;
            margin-bottom: 30px;
            font-size: 24px;
        }
        
        .error-message {
            background: #fee;
            border-left: 4px solid #e74c3c;
            color: #c0392b;
            padding: 12px;
            margin-bottom: 20px;
            border-radius: 4px;
            font-size: 14px;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        
        .error-message::before {
            content: "⚠️";
            font-size: 16px;
        }
        
        .form-group {
            margin-bottom: 20px;
        }
        
        label {
            display: block;
            margin-bottom: 8px;
            color: #333;
            font-weight: 500;
            font-size: 14px;
        }
        
        input[type="text"],
        input[type="number"],
        textarea {
            width: 100%;
            padding: 12px;
            border: 1px solid #ddd;
            border-radius: 5px;
            font-size: 14px;
            font-family: inherit;
            transition: border-color 0.3s;
        }
        
        input[type="text"]:focus,
        input[type="number"]:focus,
        textarea:focus {
            outline: none;
            border-color: #667eea;
            box-shadow: 0 0 5px rgba(102, 126, 234, 0.3);
        }
        
        textarea {
            resize: vertical;
            min-height: 120px;
        }
        
        .button-group {
            display: flex;
            gap: 10px;
            margin-top: 30px;
        }
        
        button,
        .cancel-btn {
            flex: 1;
            padding: 12px;
            border: none;
            border-radius: 5px;
            font-size: 16px;
            font-weight: 600;
            cursor: pointer;
            transition: transform 0.2s, box-shadow 0.2s;
            text-decoration: none;
            text-align: center;
            display: inline-block;
        }
        
        button {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }
        
        button:hover {
            transform: translateY(-2px);
            box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
        }
        
        button:active {
            transform: translateY(0);
        }
        
        .cancel-btn {
            background: #e0e0e0;
            color: #333;
        }
        
        .cancel-btn:hover {
            background: #d0d0d0;
        }
    </style>
</head>
<body>
    <header>
        <div class="header-content">
            <h1>🏆 Create Auction</h1>
            <a href="${pageContext.request.contextPath}/home" class="back-link">← Back to Home</a>
        </div>
    </header>
    
    <div class="container">
        <div class="form-card">
            <h2>Create New Auction</h2>
            
            <% if (errorMessage != null) { %>
                <div class="error-message"><%= errorMessage %></div>
            <% } %>
            
            <form method="POST" action="${pageContext.request.contextPath}/create-auction">
                <div class="form-group">
                    <label for="title">Auction Title:</label>
                    <input type="text" id="title" name="title" required autofocus placeholder="e.g., Vintage Camera">
                </div>
                
                <div class="form-group">
                    <label for="description">Description:</label>
                    <textarea id="description" name="description" placeholder="Describe your item in detail..."></textarea>
                </div>
                
                <div class="form-group">
                    <label for="startingPrice">Starting Price ($):</label>
                    <input type="number" id="startingPrice" name="startingPrice" required step="0.01" min="0" placeholder="0.00">
                </div>
                
                <div class="form-group">
                    <label for="minBidIncrement">Minimum Bid Increment ($):</label>
                    <input type="number" id="minBidIncrement" name="minBidIncrement" required step="0.01" min="0.01" placeholder="0.00">
                </div>
                
                <div class="form-group">
                    <label for="countdownTimer">Bid Countdown Timer (minutes):</label>
                    <input type="number" id="countdownTimer" name="countdownTimer" required min="1" placeholder="10">
                </div>
                
                <div class="form-group">
                    <label for="startDate">Start Date:</label>
                    <input type="date" id="startDate" name="startDate" required>
                </div>
                
                <div class="form-group">
                    <label for="startTime">Start Time:</label>
                    <input type="time" id="startTime" name="startTime" required>
                </div>
                
                <div class="button-group">
                    <button type="submit">Create Auction</button>
                    <a href="${pageContext.request.contextPath}/home" class="cancel-btn">Cancel</a>
                </div>
            </form>
        </div>
    </div>
</body>
</html>
