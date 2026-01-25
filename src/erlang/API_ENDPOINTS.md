# POST API Endpoints

## 1. Register Auction

**Endpoint:** `POST /api/auction`

**Description:** Registers a new active auction in the system

**Request Body:**
```json
{
  "id": "auction_001",
  "starting_price": 100.0,
  "min_duration": 600
}
```

**Response:**
```json
{
  "success": true,
  "auction_id": "auction_001"
}
```

**Example cURL:**
```bash
curl -X POST http://localhost:8080/api/auction \
  -H "Content-Type: application/json" \
  -d '{
    "id": "auction_001",
    "starting_price": 100.0,
    "min_duration": 600
  }'
```

## 2. Register User for Auction

**Endpoint:** `POST /api/user`

**Description:** Registers a user as active for a specific auction with their balance

**Request Body:**
```json
{
  "auction_id": "auction_001",
  "userid": "user123",
  "balance": 500.0
}
```

**Response:**
```json
{
  "success": true,
  "auction_id": "auction_001",
  "userid": "user123"
}
```

**Example cURL:**
```bash
curl -X POST http://localhost:8080/api/user \
  -H "Content-Type: application/json" \
  -d '{
    "auction_id": "auction_001",
    "userid": "user123",
    "balance": 500.0
  }'
```

## Changes Made

### Removed Endpoints (WebSocket):
- `add_balance` - No longer available
- `withdraw_balance` - No longer available
- `create_auction` - No longer available
- `get_waiting_auctions` - No longer available

### Removed from UI (test_client.html):
- Balance operations section (add/withdraw)
- Create auction form
- Get waiting auctions button

### How the System Works Now:

1. **External System Sends Auctions**: The external system POSTs auction data to `/api/auction` with id, starting_price, and min_duration
2. **Auction Becomes Active**: The auction is immediately created and becomes active
3. **Users are Registered**: External system POSTs user data to `/api/user` for each user participating in an auction
4. **Users Connect via WebSocket**: Users connect with JWT token and can participate in the auction
5. **Bidding Happens**: Users place bids through WebSocket as before

### WebSocket Endpoints Still Available:
- `get_balance` - Check user balance
- `connect_auction` - Connect to an active auction
- `place_bid` - Place a bid on an auction
- `get_active_auctions` - Get list of active auctions
- `get_completed_auctions` - Get list of completed auctions
