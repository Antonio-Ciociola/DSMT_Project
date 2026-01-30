<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%
    String username = (String) session.getAttribute("username");
    Integer userId = (Integer) session.getAttribute("userId");
    boolean isGuest = (username == null);
    pageContext.setAttribute("isGuest", isGuest);
    pageContext.setAttribute("username", username);
    pageContext.setAttribute("currentUserId", userId);
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
    <script>
        function formatTime(seconds) {
            const hours = Math.floor(seconds / 3600);
            const minutes = Math.floor((seconds % 3600) / 60);
            const secs = seconds % 60;
            
            const parts = [];
            if (hours > 0) parts.push(hours + 'h');
            if (minutes > 0) parts.push(minutes + 'm');
            if (secs > 0 || parts.length === 0) parts.push(secs + 's');
            
            return parts.join(' ');
        }
        
        function formatDate(dateStr) {
            // dateStr is in format "2024-01-27T15:30:00" from LocalDateTime
            // We need to treat it as UTC and convert to local time
            const utcDate = new Date(dateStr + 'Z'); // Add 'Z' to indicate UTC
            const options = {
                year: 'numeric',
                month: '2-digit',
                day: '2-digit',
                hour: '2-digit',
                minute: '2-digit',
                second: '2-digit'
            };
            return utcDate.toLocaleString('en-GB', options).replace(',', '');
        }
        
        // Convert UTC dates to local time on page load
        window.addEventListener('DOMContentLoaded', function() {
            document.querySelectorAll('[data-utc-date]').forEach(function(element) {
                const utcDate = element.getAttribute('data-utc-date');
                if (utcDate && utcDate !== '') {
                    element.textContent = formatDate(utcDate);
                }
            });
        });
        
        function joinAuction(auctionId) {
            console.log('Joining auction:', auctionId);
            
            fetch('${pageContext.request.contextPath}/join-auction', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                body: 'auctionId=' + encodeURIComponent(auctionId)
            })
            .then(response => response.json())
            .then(data => {
                console.log('Response:', data);
                if (data.success) {
                    // Store JWT token and WebSocket URL for this auction
                    localStorage.setItem('jwtToken_' + auctionId, data.jwtToken);
                    localStorage.setItem('websocketUrl_' + auctionId, data.websocketUrl);
                    console.log('Stored JWT and WebSocket URL for auction ' + auctionId);
                    
                    // Successfully joined - redirect to auction page
                    window.location.href = '${pageContext.request.contextPath}/auction/' + auctionId;
                } else {
                    // Show error message
                    alert('Error: ' + (data.error || 'Failed to join auction'));
                }
            })
            .catch(error => {
                console.error('Error:', error);
                alert('Error joining auction: ' + error.message);
            });
        }
    </script>
</head>
<body>
    <header>
        <div class="header-content">
            <h1>📦 Browse Auctions</h1>
            <a href="${pageContext.request.contextPath}/home" class="back-link">← Back to Home</a>
        </div>
    </header>
    
    <div class="container">
        <c:if test="${not empty errorMessage}">
            <div class="error-message">${errorMessage}</div>
        </c:if>
        
        <c:choose>
            <c:when test="${empty auctions}">
                <div class="no-auctions">
                    <p>No auctions available at the moment.</p>
                </div>
            </c:when>
            <c:otherwise>
                <div class="auctions-grid">
                    <c:forEach var="auction" items="${auctions}">
                        <div class="auction-card">
                            <div class="auction-header">
                                <div class="auction-title">${auction.title}</div>
                                <div class="auction-seller">Seller: <strong>${auction.ownerUsername}</strong></div>
                            </div>
                            
                            <div class="auction-body">
                                <c:if test="${not empty auction.description}">
                                    <div class="auction-description">${auction.description}</div>
                                </c:if>
                                
                                <div class="auction-details">
                                    <div class="detail-item">
                                        <div class="detail-label">Starting Price</div>
                                        <div class="detail-value price">$${String.format("%.2f", auction.startingPrice)}</div>
                                    </div>
                                    
                                    <div class="detail-item">
                                        <div class="detail-label">Min Bid Inc.</div>
                                        <div class="detail-value">$${String.format("%.2f", auction.minBidIncrement)}</div>
                                    </div>
                                    
                                    <div class="detail-item">
                                        <div class="detail-label">Starting Duration</div>
                                        <div class="detail-value">
                                            <script>document.write(formatTime(${auction.startingDuration}));</script>
                                        </div>
                                    </div>
                                    
                                    <c:choose>
                                        <c:when test="${auction.status == 'finished'}">
                                            <div class="detail-item">
                                                <div class="detail-label">Total Duration</div>
                                                <div class="detail-value">
                                                    <script>document.write(formatTime(${auction.totalDuration}));</script>
                                                </div>
                                            </div>
                                        </c:when>
                                        <c:otherwise>
                                            <div class="detail-item">
                                                <div class="detail-label">Bid Time Increment</div>
                                                <div class="detail-value">
                                                    <script>document.write(formatTime(${auction.bidTimeIncrement}));</script>
                                                </div>
                                            </div>
                                        </c:otherwise>
                                    </c:choose>
                                    
                                    <div class="detail-item">
                                        <div class="detail-label">Status</div>
                                        <div class="detail-value">${auction.status}</div>
                                    </div>
                                    
                                    <div class="detail-item">
                                        <div class="detail-label">Start Date</div>
                                        <div class="detail-value" data-utc-date="${auction.startDate}"></div>
                                    </div>
                                    
                                    <c:if test="${auction.status == 'finished'}">
                                        <div class="detail-item">
                                            <div class="detail-label">Winner</div>
                                            <div class="detail-value">
                                                <c:choose>
                                                    <c:when test="${not empty auction.winnerUsername}">
                                                        <strong>${auction.winnerUsername}</strong>
                                                    </c:when>
                                                    <c:otherwise>
                                                        No Winner
                                                    </c:otherwise>
                                                </c:choose>
                                            </div>
                                        </div>
                                        
                                        <c:if test="${not empty auction.finalPrice and auction.finalPrice > 0}">
                                            <div class="detail-item">
                                                <div class="detail-label">Final Price</div>
                                                <div class="detail-value price">$${String.format("%.2f", auction.finalPrice)}</div>
                                            </div>
                                        </c:if>
                                    </c:if>
                                </div>
                            </div>
                            
                            <c:if test="${auction.status == 'ongoing'}">
                                <div class="auction-footer">
                                    <c:choose>
                                        <c:when test="${not empty currentAuctionIdBidding and currentAuctionIdBidding == auction.id}">
                                            <button class="bid-btn" onclick="joinAuction(${auction.id})" style="background: linear-gradient(135deg, #28a745 0%, #20c997 100%);">
                                                ⚡ Active Auction
                                            </button>
                                            <div style="margin-top: 8px; font-size: 12px; color: #28a745; text-align: center;">
                                                You are currently participating in this auction
                                            </div>
                                        </c:when>
                                        <c:when test="${not empty currentAuctionIdBidding and currentAuctionIdBidding != auction.id}">
                                            <button class="bid-btn" onclick="joinAuction(${auction.id})" style="background: #6c757d;">
                                                👁️ View Only
                                            </button>
                                            <div style="margin-top: 8px; font-size: 12px; color: #666; text-align: center;">
                                                You are active in another auction
                                            </div>
                                        </c:when>
                                        <c:when test="${currentUserId == auction.userId}">
                                            <button class="bid-btn" onclick="joinAuction(${auction.id})" style="background: #6c757d;">
                                                🏠 View (Owner)
                                            </button>
                                            <div style="margin-top: 8px; font-size: 12px; color: #666; text-align: center;">
                                                You own this auction
                                            </div>
                                        </c:when>
                                        <c:otherwise>
                                            <button class="bid-btn" onclick="joinAuction(${auction.id})">
                                                <c:choose>
                                                    <c:when test="${isGuest}">👁️ View</c:when>
                                                    <c:otherwise>🎯 Join</c:otherwise>
                                                </c:choose>
                                            </button>
                                        </c:otherwise>
                                    </c:choose>
                                </div>
                            </c:if>
                        </div>
                    </c:forEach>
                </div>
            </c:otherwise>
        </c:choose>
    </div>
</body>
</html>
