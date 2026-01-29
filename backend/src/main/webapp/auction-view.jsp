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
        
        /* Auction End Overlay Styles */
        .auction-end-overlay {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.95);
            z-index: 9999;
            overflow-y: auto;
        }
        
        .auction-end-content {
            max-width: 800px;
            margin: 50px auto;
            padding: 40px;
            color: white;
            text-align: center;
        }
        
        .end-icon {
            font-size: 120px;
            margin-bottom: 30px;
            animation: bounceIn 0.8s;
        }
        
        .end-title {
            font-size: 48px;
            font-weight: 700;
            margin-bottom: 20px;
            animation: fadeInUp 0.6s;
        }
        
        .end-subtitle {
            font-size: 24px;
            opacity: 0.9;
            margin-bottom: 40px;
            animation: fadeInUp 0.8s;
        }
        
        .winner-card {
            background: linear-gradient(135deg, #f6d365 0%, #fda085 100%);
            padding: 40px;
            border-radius: 15px;
            margin-bottom: 30px;
            animation: scaleIn 0.5s;
        }
        
        .loser-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 40px;
            border-radius: 15px;
            margin-bottom: 30px;
            animation: scaleIn 0.5s;
        }
        
        .no-winner-card {
            background: linear-gradient(135deg, #868f96 0%, #596164 100%);
            padding: 40px;
            border-radius: 15px;
            margin-bottom: 30px;
            animation: scaleIn 0.5s;
        }
        
        .winner-card .card-title,
        .loser-card .card-title,
        .no-winner-card .card-title {
            font-size: 32px;
            font-weight: 700;
            margin-bottom: 15px;
        }
        
        .winner-card .winning-amount {
            font-size: 64px;
            font-weight: 900;
            margin: 20px 0;
        }
        
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 20px;
            margin: 30px 0;
            animation: fadeInUp 1s;
        }
        
        .stat-item {
            background: rgba(255, 255, 255, 0.1);
            padding: 20px;
            border-radius: 10px;
            backdrop-filter: blur(10px);
        }
        
        .stat-label {
            font-size: 14px;
            opacity: 0.8;
            margin-bottom: 5px;
        }
        
        .stat-value {
            font-size: 28px;
            font-weight: 700;
        }
        
        .end-actions {
            margin-top: 40px;
            display: flex;
            gap: 20px;
            justify-content: center;
            animation: fadeInUp 1.2s;
        }
        
        .end-button {
            padding: 15px 40px;
            border: none;
            border-radius: 8px;
            font-size: 18px;
            font-weight: 600;
            cursor: pointer;
            transition: transform 0.2s;
            text-decoration: none;
            display: inline-block;
        }
        
        .end-button:hover {
            transform: translateY(-3px);
        }
        
        .end-button.primary {
            background: white;
            color: #667eea;
        }
        
        .end-button.secondary {
            background: rgba(255, 255, 255, 0.2);
            color: white;
        }
        
        @keyframes bounceIn {
            0% { transform: scale(0); opacity: 0; }
            50% { transform: scale(1.1); }
            100% { transform: scale(1); opacity: 1; }
        }
        
        @keyframes fadeInUp {
            from { opacity: 0; transform: translateY(30px); }
            to { opacity: 1; transform: translateY(0); }
        }
        
        @keyframes scaleIn {
            from { transform: scale(0.8); opacity: 0; }
            to { transform: scale(1); opacity: 1; }
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
                
                <div class="info-card">
                    <div class="info-label">Your Balance</div>
                    <div class="info-value highlight" id="userBalance">$0.00</div>
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
    
    <!-- Auction End Overlay -->
    <div id="auctionEndOverlay" class="auction-end-overlay">
        <div class="auction-end-content">
            <div class="end-icon" id="endIcon">🏆</div>
            <div class="end-title" id="endTitle">Auction Ended!</div>
            <div class="end-subtitle" id="endSubtitle">The auction has concluded</div>
            
            <div id="endCard"></div>
            
            <div class="stats-grid">
                <div class="stat-item">
                    <div class="stat-label">Total Bids</div>
                    <div class="stat-value" id="endBidCount">0</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Participants</div>
                    <div class="stat-value" id="endParticipantCount">0</div>
                </div>
                <div class="stat-item">
                    <div class="stat-label">Duration</div>
                    <div class="stat-value" id="endDuration">0:00</div>
                </div>
            </div>
            
            <div class="end-actions">
                <a href="${pageContext.request.contextPath}/browse-auctions" class="end-button primary">Browse Auctions</a>
                <a href="${pageContext.request.contextPath}/home" class="end-button secondary">Go to Dashboard</a>
            </div>
        </div>
    </div>
    
    <script>


        // ========================================
        // Log script load time, initialize variables 
        // ========================================

        console.log('========================================');
        console.log('SCRIPT LOADED AT:', new Date().toISOString());
        console.log('========================================');
        
        let ws = null;
        const auctionId = 'auction_${auction.id}';
        const numericAuctionId = '${auction.id}';
        let timerInterval = null;
        let remainingTime = 0;
        let auctionEndTime = null;  // Absolute timestamp when auction ends
        let serverTimeDiff = 0;     // Difference between server and client time
        let keepAliveInterval = null;
        let rttSamples = [];        // Round-trip time samples for latency estimation
        let pingTimestamp = null;   // Timestamp when ping was sent
        let ntpOffsetSamples = [];  // NTP clock offset samples
        let ntpSyncInProgress = false;  // Flag to prevent concurrent sync requests
        
        console.log('Variables initialized:');
        console.log('  auctionId:', auctionId);
        console.log('  numericAuctionId:', numericAuctionId);
        console.log('========================================');


        // =======================================
        // Functions Definition
        // =======================================
        
        // Decode JWT to check if user is guest
        // Returns payload object or null on failure
        function decodeJWT(token) {
            try {
                const parts = token.split('.');
                if (parts.length !== 3) return null;
                const payload = JSON.parse(atob(parts[1]));
                return payload;
            } catch (e) {
                console.error('Failed to decode JWT:', e);
                return null;
            }
        }
        
        // Update UI for guest mode
        // Hides bid controls and shows appropriate message
        function updateUIForGuestMode(isGuest) {

            // Hides bid controls and shows appropriate message
            if (isGuest) {

                // Log guest mode detection
                console.log('Guest mode detected - hiding bid controls');
                
                // Hide balance and bid section
                const balanceCard = document.querySelector('.info-card:has(#userBalance)');
                if (balanceCard) balanceCard.style.display = 'none';
                
                // Update bid section
                const bidSection = document.querySelector('.bid-section');
                if (bidSection) {

                    // Check JWT to determine reason for guest mode
                    const token = localStorage.getItem('jwtToken_' + numericAuctionId);
                    let message = 'Viewing as Guest - You are viewing this auction in read-only mode. Log in to place bids.';
                    
                    // Check if user is owner or active in another auction
                    if (token) {
                        const payload = decodeJWT(token);
                        if (payload && payload.sub) {
                            const username = payload.sub;
                            if (username.startsWith('guest_owner_')) {
                                message = 'Owner View - You cannot bid on your own auction.';
                            } else if (username.startsWith('guest_')) {
                                message = 'View Only - You are currently active in another auction.';
                            }
                        }
                    }
                    
                    // Update bid section with appropriate message
                    bidSection.innerHTML = '<h3>Viewing only</h3><p style="margin-top: 10px; color: #666;">' + message + '</p>';
                }
            }
        }
      
        // Open WebSocket connection and handle events
        // Includes automatic reconnection logic and RTT/NTP time sync
        // Also handles auction joining if not already joined
        function connect() {

            // Get auction-specific JWT token and WebSocket URL
            const token = localStorage.getItem('jwtToken_' + numericAuctionId);
            const wsUrl = localStorage.getItem('websocketUrl_' + numericAuctionId);
            
            // Log connection attempt
            console.log('Auction ID:', numericAuctionId);
            console.log('Token from localStorage:', token ? 'Found' : 'Not found');
            console.log('WebSocket URL from localStorage:', wsUrl);
            
            // Check if guest mode
            if (token) {
                const payload = decodeJWT(token);
                if (payload && payload.guest === true) {
                    updateUIForGuestMode(true);
                }
            }
            
            // Not joined yet - join automatically
            if (!token || !wsUrl) {
                console.log('Not joined yet, joining auction automatically...');
                joinAuctionAutomatically();
                return;
            }
            
            // Append JWT token as query parameter and connect
            const wsUrlWithToken = wsUrl + '?token=' + encodeURIComponent(token);
            console.log('Connecting to WebSocket:', wsUrlWithToken);
            updateConnectionStatus('connecting', 'Connecting to auction...');
            ws = new WebSocket(wsUrlWithToken);
            
            // WebSocket event handlers
            ws.onopen = () => {
                console.log('WebSocket CONNECTED');
                console.log('WebSocket.readyState:', ws.readyState);
                console.log('WebSocket.url:', ws.url);
                updateConnectionStatus('connected', 'Connected to auction');
                
                // Connection and auction registration happens automatically in Erlang
                // based on the JWT token (which contains user_id, auction_id, and balance)
                
                // Start keepalive - send ping every 30 seconds with timestamp for RTT measurement
                // Also perform NTP sync periodically (every other ping)
                keepAliveInterval = setInterval(() => {
                    if (ws && ws.readyState === WebSocket.OPEN) {
                        pingTimestamp = performance.now();
                        ws.send(JSON.stringify({type: 'ping', timestamp: pingTimestamp}));
                        console.log('Sent keepalive ping at', pingTimestamp);
                        
                        // Also perform NTP sync periodically (every other ping)
                        if (Math.random() < 0.5) {
                            setTimeout(() => performNTPSync(), 500);
                        }
                    }
                }, 30000);
            };
           
            // Handle incoming messages
            ws.onmessage = (event) => {

                // Log raw and parsed message
                console.log('RAW MESSAGE:', event.data);
                const msg = JSON.parse(event.data);
                console.log('PARSED MESSAGE:', msg);
                console.log('Message type:', msg.type);
                
                // If auction update, update auction info
                if (msg.type === 'auction_update') {
                    console.log('Processing auction_update');
                    updateAuctionInfo(msg.auction_state);
                }        
                // Handle connected confirmation
                else if (msg.type === 'connected') {
                    console.log('Received connected confirmation');
                    const bidButton = document.getElementById('bidButton');
                    if (bidButton) {
                        bidButton.disabled = false;
                    }
                    
                    // Perform initial NTP time sync
                    setTimeout(() => performNTPSync(), 100);
                    
                    // Request user balance (only if not guest)
                    const token = localStorage.getItem('jwtToken_' + numericAuctionId);
                    const payload = token ? decodeJWT(token) : null;
                    if (!payload || !payload.guest) {
                        send({type: 'get_balance'});
                    }
                } 
                // Handle auction ended message
                else if (msg.type === 'auction_ended') {
                    console.log('Auction ended');
                    handleAuctionEnded(msg);
                } 
                // Handle keepalive pong response
                else if (msg.type === 'pong') {
                    console.log('Received keepalive pong');
                    
                    // Measure round-trip time for latency compensation
                    if (msg.timestamp && msg.timestamp === pingTimestamp) {
                        const rtt = performance.now() - pingTimestamp;
                        rttSamples.push(rtt);
                        
                        // Keep last 10 samples for statistical accuracy
                        if (rttSamples.length > 10) {
                            rttSamples.shift();
                        }
                        
                        // Calculate median RTT (more robust than average)
                        const sortedRTT = [...rttSamples].sort((a, b) => a - b);
                        const medianRTT = sortedRTT[Math.floor(sortedRTT.length / 2)];
                        const oneWayLatency = medianRTT / 1000; // Convert to seconds
                        
                        console.log('RTT:', rtt.toFixed(2), 'ms, Median:', medianRTT.toFixed(2), 'ms, One-way:', (oneWayLatency * 1000).toFixed(2), 'ms');
                        
                        pingTimestamp = null;
                    }
                } 
                // Handle NTP-style time sync response
                else if (msg.type === 'time_sync_response') {
                    console.log('Received NTP-style time sync response');
                    handleTimeSyncResponse(msg);
                } 
                // Handle error placed messages
                else if (msg.type === 'error') {
                    console.error('Error message:', msg.message);
                    showMessage(msg.message || 'An error occurred', 'error');
                } 
                // Handle bid placed confirmation
                else if (msg.type === 'bid_placed') {
                    console.log('Bid placed successfully');
                    showMessage('Bid placed successfully!', 'success');
                } 
                // Handle balance response
                else if (msg.type === 'balance_response') {
                    console.log('Balance response:', msg.balance);
                    updateBalance(msg.balance);
                } 
                // Unknown message type
                else {
                    console.warn('Unknown message type:', msg.type);
                }
            };
            
            // Handle WebSocket errors
            ws.onerror = (error) => {
                console.error('WebSocket error:', error);
                console.error('WebSocket readyState:', ws ? ws.readyState : 'null');
                console.error('WebSocket URL was:', ws ? ws.url : 'unknown');
                updateConnectionStatus('disconnected', 'Connection error');
                showMessage('WebSocket connection error', 'error');
            };
            
            // Handle WebSocket closure and attempt reconnection
            ws.onclose = () => {
                console.log('WebSocket DISCONNECTED');
                console.log('Close event - readyState:', ws ? ws.readyState : 'null');
                console.log('Will attempt reconnect in 5 seconds...');
                updateConnectionStatus('disconnected', 'Disconnected from auction');
                document.getElementById('bidButton').disabled = true;
                
                // Clear keepalive interval
                if (keepAliveInterval) {
                    clearInterval(keepAliveInterval);
                    keepAliveInterval = null;
                }
                
                // Try to reconnect after 5 seconds
                setTimeout(connect, 5000);
            };
        }
        
        // Send message via WebSocket with error handling
        function send(msg) {
            console.log('SENDING:', msg);
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(msg));
                console.log('Message sent successfully');
            } else {
                console.error('Cannot send - WebSocket not open. ReadyState:', ws ? ws.readyState : 'null');
                showMessage('Not connected to auction', 'error');
            }
        }
        
        // NTP-style time synchronization using formula: offset = ½((T2 - T1) + (T3 - T4))
        // Where: T1 = client send, T2 = server receive, T3 = server send, T4 = client receive
        function performNTPSync() {

            // Prevent concurrent sync requests
            if (ntpSyncInProgress || !ws || ws.readyState !== WebSocket.OPEN) {
                return;
            }
            
            // Start NTP sync
            ntpSyncInProgress = true;
            const t1 = Date.now(); // Ti-3: Client send timestamp
            
            // Send time sync request
            send({ type: 'time_sync', t1: t1 });

        }
        
        // Handle NTP-style time sync response from server
        function handleTimeSyncResponse(msg) {

            // Calculate NTP offset and delay
            const t4 = Date.now(); // Ti: Client receive timestamp
            const t1 = msg.t1;     // Ti-3: Client send timestamp (echoed back)
            const t2 = msg.t2;     // Ti-2: Server receive timestamp
            const t3 = msg.t3;     // Ti-1: Server send timestamp
            
            // NTP clock offset formula: offset = 1/2 ((T2 - T1) + (T3 - T4))
            // This is equivalent to: offset = 1/2 (Ti-2 - Ti-3 + Ti-1 - Ti)
            const offset = 0.5 * ((t2 - t1) + (t3 - t4));
            
            // Round-trip delay: d = (T4 - T1) - (T3 - T2)
            // computed only for information purposes
            const delay = (t4 - t1) - (t3 - t2);
            
            // Store offset sample
            ntpOffsetSamples.push(offset);
            
            // Keep last 10 samples
            if (ntpOffsetSamples.length > 10) {
                ntpOffsetSamples.shift();
            }
            
            // Calculate median offset (robust to outliers), used for logging here
            const sortedOffsets = [...ntpOffsetSamples].sort((a, b) => a - b);
            const medianOffset = sortedOffsets[Math.floor(sortedOffsets.length / 2)];
            
            // Log NTP sync results
            console.log(' NTP Sync:', {
                t1: t1, t2: t2, t3: t3, t4: t4,
                offset: offset.toFixed(2) + 'ms',
                delay: delay.toFixed(2) + 'ms',
                medianOffset: medianOffset.toFixed(2) + 'ms',
                samples: ntpOffsetSamples.length
            });
            
            // Update server time difference using NTP offset (convert ms to seconds)
            serverTimeDiff = medianOffset / 1000;

            // Mutex release
            ntpSyncInProgress = false;
        }
        
        // Place bid function - called when user clicks "Place Bid" button
        function placeBid() {

            // Get bid amount from input
            const amount = parseFloat(document.getElementById('bidAmount').value);
            
            // Validate bid amount
            if (!amount || amount <= 0) {
                showMessage('Please enter a valid bid amount', 'error');
                return;
            }
           
            // Send place_bid message
            send({
                type: 'place_bid',
                auction_id: auctionId,
                amount: amount
            });
        }
        
        // Update auction info in the UI based on server state
        function updateAuctionInfo(state) {

            
            // Update time sync when receiving server updates (only if not initialized or significant drift)
            if (state.server_time && state.auction_end_time) {

                auctionEndTime = state.auction_end_time;
            }
            
            
            // Start local countdown timer if not already running
            if (!timerInterval && auctionEndTime) {
                timerInterval = setInterval(() => {
                    const currentTime = Math.floor(Date.now() / 1000) + serverTimeDiff;
                    remainingTime = Math.max(0, auctionEndTime - currentTime);
                    updateTimerDisplay();
                    
                    if (remainingTime <= 0) {
                        clearInterval(timerInterval);
                        timerInterval = null;
                    }
                }, 1000);
                
                // Update immediately
                const currentTime = Math.floor(Date.now() / 1000) + serverTimeDiff;
                remainingTime = Math.max(0, auctionEndTime - currentTime);
                updateTimerDisplay();
            }
            
            
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
                
                // Suggest next bid amount (only if bidAmount element exists - not for guests)
                const bidAmountEl = document.getElementById('bidAmount');
                if (bidAmountEl) {
                    const minNextBid = state.highest_bid.amount + ${auction.minBidIncrement};
                    bidAmountEl.value = minNextBid.toFixed(2);
                    bidAmountEl.min = minNextBid;
                }
            } else {
                document.getElementById('highestBidCard').style.display = 'none';
                
                // Set starting price (only if bidAmount element exists - not for guests)
                const bidAmountEl = document.getElementById('bidAmount');
                if (bidAmountEl) {
                    bidAmountEl.value = ${auction.startingPrice};
                    bidAmountEl.min = ${auction.startingPrice};
                }
            }
        }
        
        // Update connection status UI
        function updateTimerDisplay() {
            const minutes = Math.floor(remainingTime / 60);
            const seconds = Math.floor(remainingTime % 60);
            document.getElementById('timeRemaining').textContent = 
                minutes + ':' + String(seconds).padStart(2, '0');
        }
        
        // Update connection status UI
        function updateBalance(balance) {
            document.getElementById('userBalance').textContent = 
                '$' + (balance || 0).toFixed(2);
        }
        
        // Handle auction ended event
        function handleAuctionEnded(msg) {
            console.log('Auction ended:', msg);
            
            // Stop keepalive pings
            if (keepAliveInterval) {
                clearInterval(keepAliveInterval);
                keepAliveInterval = null;
                console.log('🛑 Stopped keepalive pings');
            }
            
            // Close WebSocket connection
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.close();
                console.log('🔌 Closed WebSocket connection');
            }
            
            // Stop timer
            if (timerInterval) {
                clearInterval(timerInterval);
                timerInterval = null;
            }
            
            // Get current username from session
            const currentUsername = '${sessionScope.username}';
            
            // Determine if current user won
            const hasWinner = msg.winner_user_id && msg.winner_user_id !== 'none' && msg.winner_user_id !== 'undefined';
            const isWinner = hasWinner && String(msg.winner_user_id) === String(currentUsername);
            
            // Calculate duration
            const durationMins = Math.floor(msg.total_duration / 60);
            const durationSecs = String(msg.total_duration % 60).padStart(2, '0');
            
            // Update statistics
            document.getElementById('endBidCount').textContent = msg.bid_count;
            document.getElementById('endParticipantCount').textContent = msg.participant_count;
            document.getElementById('endDuration').textContent = durationMins + ':' + durationSecs;
            
            // Build the result card
            let cardHtml = '';
            
            if (!hasWinner) {
                // No winner
                document.getElementById('endIcon').textContent = '📦';
                document.getElementById('endTitle').textContent = 'Auction Ended';
                document.getElementById('endSubtitle').textContent = 'No bids were placed';
                
                cardHtml = '<div class="no-winner-card">' +
                          '<div class="card-title">No Winner</div>' +
                          '<p style="font-size: 18px; opacity: 0.9;">This auction concluded without any bids.</p>' +
                          '</div>';
            } else if (isWinner) {
                // Current user won
                document.getElementById('endIcon').textContent = '🎉';
                document.getElementById('endTitle').textContent = 'Congratulations!';
                document.getElementById('endSubtitle').textContent = 'You won this auction!';
                
                cardHtml = '<div class="winner-card">' +
                          '<div class="card-title">🏆 You Are The Winner! 🏆</div>' +
                          '<div class="winning-amount">$' + msg.winning_bid.toFixed(2) + '</div>' +
                          '<p style="font-size: 20px; font-weight: 600;">Winning Bid</p>' +
                          '<p style="font-size: 16px; opacity: 0.9; margin-top: 20px;">The winning amount has been deducted from your balance.</p>' +
                          '</div>';
            } else {
                // Someone else won
                document.getElementById('endIcon').textContent = '🎯';
                document.getElementById('endTitle').textContent = 'Auction Ended';
                document.getElementById('endSubtitle').textContent = 'Better luck next time!';
                
                cardHtml = '<div class="loser-card">' +
                          '<div class="card-title">Auction Won by User ' + msg.winner_user_id + '</div>' +
                          '<div class="winning-amount">$' + msg.winning_bid.toFixed(2) + '</div>' +
                          '<p style="font-size: 18px; opacity: 0.9; margin-top: 15px;">Thank you for participating!</p>' +
                          '</div>';
            }
            
            document.getElementById('endCard').innerHTML = cardHtml;
            
            // Show overlay with slight delay for dramatic effect
            setTimeout(() => {
                document.getElementById('auctionEndOverlay').style.display = 'block';
            }, 500);
        }
        
        // Automatically join auction if not already joined
        // Sends POST request to join-auction endpoint
        function joinAuctionAutomatically() {

            // Log auto-join attempt
            console.log('AUTO-JOINING auction:', numericAuctionId);
            updateConnectionStatus('connecting', 'Joining auction...');
            
            // Send POST request to join auction
            fetch('${pageContext.request.contextPath}/join-auction', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                body: 'auctionId=' + encodeURIComponent(numericAuctionId)
            })
            // Handle response
            .then(response => {
                console.log('Join response status:', response.status);
                return response.json();
            })
            // Process response data
            .then(data => {
                console.log('Join response data:', data);
                if (data.success) {
                    console.log('Join successful');
                    console.log('JWT Token:', data.jwtToken ? data.jwtToken.substring(0, 30) + '...' : 'MISSING');
                    console.log('WebSocket URL:', data.websocketUrl);
                    // Store JWT token and WebSocket URL
                    localStorage.setItem('jwtToken_' + numericAuctionId, data.jwtToken);
                    localStorage.setItem('websocketUrl_' + numericAuctionId, data.websocketUrl);
                    console.log('Stored in localStorage, retrying connection...');
                    // Try to connect again
                    connect();
                } else {
                    console.error('Join failed:', data.error);
                    showMessage('Error: ' + (data.error || 'Failed to join auction'), 'error');
                    updateConnectionStatus('disconnected', 'Join failed');
                }
            })
            // Handle fetch errors
            .catch(error => {
                console.error('Join error:', error);
                showMessage('Error joining auction: ' + error.message, 'error');
                updateConnectionStatus('disconnected', 'Join failed');
            });
        }
        
        // Update connection status UI
        function updateConnectionStatus(status, message) {
            const statusDiv = document.getElementById('connectionStatus');
            statusDiv.className = 'connection-status ' + status;
            statusDiv.textContent = message;
        }
        
        // Show message to user in message container
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
        

        // =======================================
        // Event Listeners
        // =======================================

        // Connect to the auction web socket when page loads
        console.log('🚀 PAGE LOADED - Initializing connection...');
        window.addEventListener('load', connect);
        
        // Clean up auction web socket on page unload
        window.addEventListener('beforeunload', () => {
            console.log('🚪 Page unloading - closing WebSocket');
            if (ws) {
                ws.close();
            }
        });
        
    </script>
</body>
</html>
