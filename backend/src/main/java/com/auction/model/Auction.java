package com.auction.model;

import java.time.LocalDateTime;

public class Auction {
    private int id;
    private int userId;
    private String title;
    private String description;
    private double startingPrice;
    private double minBidIncrement;
    private LocalDateTime startDate;
    private String ownerUsername;
    private String status;
    private Integer winnerUserId;
    private String winnerUsername;
    private Double finalPrice;
    private int initialWaitTime;
    private int bidTimeIncrement;

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

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Integer getWinnerUserId() {
        return winnerUserId;
    }

    public void setWinnerUserId(Integer winnerUserId) {
        this.winnerUserId = winnerUserId;
    }

    public String getWinnerUsername() {
        return winnerUsername;
    }

    public void setWinnerUsername(String winnerUsername) {
        this.winnerUsername = winnerUsername;
    }

    public Double getFinalPrice() {
        return finalPrice;
    }

    public void setFinalPrice(Double finalPrice) {
        this.finalPrice = finalPrice;
    }

    public int getInitialWaitTime() {
        return initialWaitTime;
    }

    public void setInitialWaitTime(int initialWaitTime) {
        this.initialWaitTime = initialWaitTime;
    }

    public int getBidTimeIncrement() {
        return bidTimeIncrement;
    }

    public void setBidTimeIncrement(int bidTimeIncrement) {
        this.bidTimeIncrement = bidTimeIncrement;
    }
}
