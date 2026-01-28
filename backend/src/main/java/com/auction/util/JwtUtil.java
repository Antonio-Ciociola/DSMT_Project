package com.auction.util;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import java.security.Key;
import java.util.Date;
import java.util.Optional;

/**
 * Utility class for JWT (JSON Web Token) generation and verification.
 * Used for stateless authentication and inter-service communication.
 */
public class JwtUtil {

    // Secret key for signing tokens (should be at least 256 bits for HS256)
    // In production, this should be loaded from environment variable
    private static final String SECRET_KEY = System.getenv("JWT_SECRET") != null 
        ? System.getenv("JWT_SECRET") 
        : "your-secret-key-should-be-at-least-32-chars";

    // Token expiration time in milliseconds (24 hours)
    private static final long EXPIRATION_TIME = 24 * 60 * 60 * 1000;

    // Signing key generated from the secret
    private static final Key key = Keys.hmacShaKeyFor(SECRET_KEY.getBytes());

    /**
     * Generates a JWT token for a user participating in an auction.
     * @param username  the username to embed in the token
     * @param auctionId the auction ID the user is joining
     * @param balance   the user's balance for this auction
     * @return JWT token as String
     */
    public static String generateToken(String username, String auctionId, double balance, boolean isGuest) {
        Date now = new Date();
        Date expiration = new Date(now.getTime() + EXPIRATION_TIME);

        return Jwts.builder()
                .subject(username != null ? username : "guest")
                .claim("auctionId", auctionId)
                .claim("balance", balance)
                .claim("guest", isGuest)
                .issuedAt(now)
                .expiration(expiration)
            // JJWT 0.12+: algorithm inferred from key type; key is HMAC so HS256 is chosen
                .signWith(key)
                .compact();
    }

}
