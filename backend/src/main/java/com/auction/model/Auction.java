package com.auction.model;

import java.time.LocalDateTime;

public class Auction {
    private int id;
    private int userId;
    private String title;
    private String description;
    private double startingPrice;
    private double minBidIncrement;
    private int countdownTimer;
    private LocalDateTime startDate;
    private String ownerUsername;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getUserId() {
        return userId;
    }

    public void setUserId(int userId) {
        this.userId = userId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public double getStartingPrice() {
        return startingPrice;
    }

    public void setStartingPrice(double startingPrice) {
        this.startingPrice = startingPrice;
    }

    public double getMinBidIncrement() {
        return minBidIncrement;
    }

    public void setMinBidIncrement(double minBidIncrement) {
        this.minBidIncrement = minBidIncrement;
    }

    public int getCountdownTimer() {
        return countdownTimer;
    }

    public void setCountdownTimer(int countdownTimer) {
        this.countdownTimer = countdownTimer;
    }

    public LocalDateTime getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDateTime startDate) {
        this.startDate = startDate;
    }

    public String getOwnerUsername() {
        return ownerUsername;
    }

    public void setOwnerUsername(String ownerUsername) {
        this.ownerUsername = ownerUsername;
    }
}
