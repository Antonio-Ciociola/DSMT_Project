# Specifiche

## Project Proposal

Auction Manager is a distributed web-app in which users can sell their goods by creating Online Auctions. Registered users have the possibility to join an auction in order to buy a good in case they beat the other users by setting a higher offer on a given limited time.

### Requirements

The functional requirements below are grouped according to their respective themes.

#### User Management

- **User Registration**: Non-Registered Users must be able to create an account to become Registered Users.
- **Balance Management**: Registered Users can deposit funds into their personal accounts to maintain a digital balance.

#### Auction Creation and Management

- **Auction Creation**: Registered Users can create new auctions by specifying a scheduled start date and time.
- **Configure Auction Parameters**: Registered Users can configure, upon creation, all the auction parameters, like minimum bid and increment.
- **Auction Cancellation**: The creator of an auction (Registered User) may cancel it, provided the auction has not yet started.
- **Viewing Upcoming Auctions**: Users can browse and view a list of scheduled auctions that are pending start.

#### Participation and Bidding

- **Registration for Participation**: Each auction, before it begins, has a waiting list. Registered Users can join one waiting list, indicating their will to participate to a specific auction.
- **Restricted Access**: Once an auction starts, participation is strictly limited to those Registered Users who joined the waiting list beforehand.
- **Viewing Access**: All Users, including unregistered ones, can join an ongoing auction as a spectator and therefore view the countdown and current best offer while not being able to place an offer.
- **Bidding Process**: During a live auction, participating users can submit bids.
- **Real-time Updates**: When a new bid is placed, all participants must see the updated high bid immediately.
- **Dynamic Countdown**: Every new bid resets the auction countdown timer. The auction concludes only when the countdown reaches zero without any further bids.
- **Balance Validation**: A participant cannot place a bid that exceeds their current available balance.

These are the **actors** in our project:

- **Non Registered User**: A guest visitor who can only view public landing pages or register.
- **Registered User**:  A logged-in user with the authority to create auctions, manage a balance, and participate in bidding.

### Challenges

The implementation must address five critical challenges inherent to distributed environments:

- **Distributed Global Countdown**
  - Maintaining a unified global countdown across heterogeneous nodes presents a consistency challenge. The system must implement a mechanism to ensure all clients possess a coherent view of the remaining time.
- **Countdown Resets**
  - The countdown must be reset upon the acceptance of a new valid bid. In a distributed context, this requires an atomic broadcast of the reset signal to all participants to prevent race conditions between the "bid acceptance" and "timer expiration" events.
- **Ordering of Bids**
  - In scenarios where multiple clients submit bids almost simultaneously, the system cannot rely on physical timestamps due to the lack of a global clock. It will be necessary to implement a logical time systems that allows to ensure clock consistency across nodes.
- **Coeherent Bid Increments**
  - The system must validate that every incoming bid respects the minimum increment relative to the current highest bid. This requires that if two bids are made almost simultaneously we don't only need to check which has to be registered first but we also need to check whether the second is valid given the first.
- **Balance Verification**
  - Before a bid is committed to the global state, the system must verify that the client has sufficient funds.

### Architecture

The **front-end** is a simple web application (HTML+CSS for the presentation, JavaScript for the logic).

We have a **back-end** written in Java (leveraging a pool of thread to handle multiple concurrent connection) which interacts with a MySQL database to perform operations like login/signup, depositing funds, checking balance or creating/listing auctions.

We have a Java **load balancer** which handles the request of a connection of a client to a waiting list or to an ongoing auction, it decides which Erlang node should the client be connected to.

We have a **distributed** network of **Erlang nodes** which handle the auction: a single node handles 1+ clients (and each client is assigned only to a node) and therefore a node could hold client partaking in different auctions.

We use **Mnesia** distributed DB to support the distributed network of Erlang nodes.
