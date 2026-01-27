<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${auction.title} - Auction</title>
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
        
        .auction-container {
            background: white;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
            padding: 40px;
        }
        
        .auction-title {
            font-size: 32px;
            font-weight: 600;
            color: #333;
            margin-bottom: 20px;
            text-align: center;
        }
        
        .connection-status {
            text-align: center;
            padding: 10px;
            margin-bottom: 20px;
            border-radius: 5px;
            font-weight: 500;
        }
        
        .connected {
            background: #d4edda;
            color: #155724;
        }
        
        .disconnected {
            background: #f8d7da;
            color: #721c24;
        }
        
        .connecting {
            background: #fff3cd;
            color: #856404;
        }
        
        .auction-info {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        
        .info-card {
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            border-left: 4px solid #667eea;
        }
        
        .info-label {
            font-size: 12px;
            color: #666;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 5px;
        }
        
        .info-value {
            font-size: 24px;
            font-weight: 600;
            color: #333;
        }
        
        .info-value.highlight {
            color: #667eea;
        }
        
        .info-value.timer {
            color: #dc3545;
        }
        
        .bid-section {
            background: #f8f9fa;
            padding: 30px;
            border-radius: 8px;
            margin-top: 30px;
        }
        
        .bid-section h3 {
            color: #667eea;
            margin-bottom: 20px;
            font-size: 20px;
        }
        
        .bid-form {
            display: flex;
            gap: 10px;
            align-items: stretch;
        }
        
        .bid-input-group {
            flex: 1;
        }
        
        .bid-input-group label {
            display: block;
            margin-bottom: 5px;
            font-size: 14px;
            font-weight: 500;
            color: #333;
        }
        
        .bid-input-group input {
            width: 100%;
            padding: 12px;
            border: 2px solid #ddd;
            border-radius: 5px;
            font-size: 18px;
            font-weight: 600;
            transition: border-color 0.3s;
        }
        
        .bid-input-group input:focus {
            outline: none;
            border-color: #667eea;
        }
        
        .bid-button {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border: none;
            padding: 12px 30px;
            border-radius: 5px;
            font-size: 16px;
            font-weight: 600;
            cursor: pointer;
            transition: transform 0.2s, box-shadow 0.2s;
            align-self: flex-end;
        }
        
        .bid-button:hover:not(:disabled) {
            transform: translateY(-2px);
            box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
        }
        
        .bid-button:disabled {
            background: #ccc;
            cursor: not-allowed;
            transform: none;
        }
        
        .message {
            padding: 12px;
            margin: 10px 0;
            border-radius: 5px;
            font-size: 14px;
        }
        
        .message.success {
            background: #d4edda;
            color: #155724;
            border-left: 4px solid #28a745;
        }
        
        .message.error {
            background: #f8d7da;
            color: #721c24;
            border-left: 4px solid #dc3545;
        }
        
        .message.info {
            background: #d1ecf1;
            color: #0c5460;
            border-left: 4px solid #17a2b8;
        }
        
        .highest-bid-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 25px;
            border-radius: 8px;
            margin-bottom: 20px;
            text-align: center;
        }
        
        .highest-bid-card .label {
            font-size: 14px;
            opacity: 0.9;
            margin-bottom: 10px;
        }
        
        .highest-bid-card .amount {
            font-size: 48px;
            font-weight: 700;
            margin-bottom: 5px;
        }
        
        .highest-bid-card .bidder {
            font-size: 16px;
            opacity: 0.9;
        }
    </style>
</head>
<body>
    <header>
        <div class="header-content">
            <h1>🔨 Auction View</h1>
            <a href="${pageContext.request.contextPath}/browse-auctions" class="back-link">← Back to Browse</a>
        </div>
    </header>
    
    <div class="container">
        <div class="auction-container">
            <h2 class="auction-title">${auction.title}</h2>
            
            <div id="connectionStatus" class="connection-status connecting">
                Connecting to auction...
            </div>
            
            <div id="messageContainer"></div>
            
            <div id="highestBidCard" class="highest-bid-card" style="display: none;">
                <div class="label">Current Highest Bid</div>
                <div class="amount" id="highestBidAmount">$0.00</div>
                <div class="bidder" id="highestBidder">No bids yet</div>
            </div>
            
            <div class="auction-info">
                <div class="info-card">
                    <div class="info-label">Status</div>
                    <div class="info-value" id="auctionStatus">Loading...</div>
                </div>
                
                <div class="info-card">
                    <div class="info-label">Time Remaining</div>
                    <div class="info-value timer" id="timeRemaining">--:--</div>
                </div>
                
                <div class="info-card">
                    <div class="info-label">Starting Price</div>
                    <div class="info-value highlight" id="startingPrice">$${auction.startingPrice}</div>
                </div>
                
                <div class="info-card">
                    <div class="info-label">Min Bid Increment</div>
                    <div class="info-value highlight" id="minBidIncrement">$${auction.minBidIncrement}</div>
                </div>
                
                <div class="info-card">
                    <div class="info-label">Total Bids</div>
                    <div class="info-value" id="bidCount">0</div>
                </div>
                
                <div class="info-card">
                    <div class="info-label">Participants</div>
                    <div class="info-value" id="participantCount">0</div>
                </div>
            </div>
            
            <div class="bid-section">
                <h3>Place Your Bid</h3>
                <div class="bid-form">
                    <div class="bid-input-group">
                        <label for="bidAmount">Bid Amount ($)</label>
                        <input type="number" id="bidAmount" step="0.01" min="0" placeholder="Enter bid amount">
                    </div>
                    <button class="bid-button" id="bidButton" onclick="placeBid()" disabled>Place Bid</button>
                </div>
            </div>
        </div>
    </div>
    
    <script>
        let ws = null;
        const auctionId = 'auction_${auction.id}';
        
        function connect() {
            const token = localStorage.getItem('jwtToken');
            
            if (!token) {
                showMessage('Please log in to participate in the auction', 'error');
                updateConnectionStatus('disconnected', 'Not authenticated');
                return;
            }
            
            const wsUrl = 'ws://localhost:8080/ws?token=' + encodeURIComponent(token);
            updateConnectionStatus('connecting', 'Connecting to auction...');
            
            ws = new WebSocket(wsUrl);
            
            ws.onopen = () => {
                console.log('WebSocket connected');
                updateConnectionStatus('connected', 'Connected to auction');
                
                // Connect to the specific auction as a participant
                send({
                    type: 'connect_auction',
                    auction_id: auctionId,
                    role: 'participant'
                });
            };
            
            ws.onmessage = (event) => {
                const msg = JSON.parse(event.data);
                console.log('Received message:', msg);
                
                if (msg.type === 'auction_update') {
                    updateAuctionInfo(msg.auction_state);
                } else if (msg.type === 'error') {
                    showMessage(msg.message || 'An error occurred', 'error');
                } else if (msg.type === 'bid_placed') {
                    showMessage('Bid placed successfully!', 'success');
                } else if (msg.type === 'connection_confirmed') {
                    showMessage('Successfully joined auction', 'success');
                    document.getElementById('bidButton').disabled = false;
                }
            };
            
            ws.onerror = (error) => {
                console.error('WebSocket error:', error);
                updateConnectionStatus('disconnected', 'Connection error');
                showMessage('WebSocket connection error', 'error');
            };
            
            ws.onclose = () => {
                console.log('WebSocket disconnected');
                updateConnectionStatus('disconnected', 'Disconnected from auction');
                document.getElementById('bidButton').disabled = true;
                
                // Try to reconnect after 5 seconds
                setTimeout(connect, 5000);
            };
        }
        
        function send(msg) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(msg));
            } else {
                showMessage('Not connected to auction', 'error');
            }
        }
        
        function placeBid() {
            const amount = parseFloat(document.getElementById('bidAmount').value);
            
            if (!amount || amount <= 0) {
                showMessage('Please enter a valid bid amount', 'error');
                return;
            }
            
            send({
                type: 'place_bid',
                auction_id: auctionId,
                amount: amount
            });
        }
        
        function updateAuctionInfo(state) {
            // Update status
            document.getElementById('auctionStatus').textContent = state.status || 'active';
            
            // Update time remaining
            const minutes = Math.floor(state.remaining_time / 60);
            const seconds = state.remaining_time % 60;
            document.getElementById('timeRemaining').textContent = 
                minutes + ':' + String(seconds).padStart(2, '0');
            
            // Update bid count
            document.getElementById('bidCount').textContent = state.bid_count || 0;
            
            // Update participant count
            document.getElementById('participantCount').textContent = state.participant_count || 0;
            
            // Update highest bid
            if (state.highest_bid && state.highest_bid !== 'none') {
                document.getElementById('highestBidCard').style.display = 'block';
                document.getElementById('highestBidAmount').textContent = 
                    '$' + state.highest_bid.amount.toFixed(2);
                document.getElementById('highestBidder').textContent = 
                    'by ' + state.highest_bid.username;
                
                // Suggest next bid amount
                const minNextBid = state.highest_bid.amount + ${auction.minBidIncrement};
                document.getElementById('bidAmount').value = minNextBid.toFixed(2);
                document.getElementById('bidAmount').min = minNextBid;
            } else {
                document.getElementById('highestBidCard').style.display = 'none';
                document.getElementById('bidAmount').value = ${auction.startingPrice};
                document.getElementById('bidAmount').min = ${auction.startingPrice};
            }
        }
        
        function updateConnectionStatus(status, message) {
            const statusDiv = document.getElementById('connectionStatus');
            statusDiv.className = 'connection-status ' + status;
            statusDiv.textContent = message;
        }
        
        function showMessage(text, type) {
            const container = document.getElementById('messageContainer');
            const msgDiv = document.createElement('div');
            msgDiv.className = 'message ' + type;
            msgDiv.textContent = text;
            container.appendChild(msgDiv);
            
            // Auto-remove after 5 seconds
            setTimeout(() => {
                msgDiv.remove();
            }, 5000);
        }
        
        // Connect when page loads
        window.addEventListener('load', connect);
        
        // Clean up on page unload
        window.addEventListener('beforeunload', () => {
            if (ws) {
                ws.close();
            }
        });
    </script>
</body>
</html>
