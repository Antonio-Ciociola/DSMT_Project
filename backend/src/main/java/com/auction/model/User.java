package com.auction.model;

import java.math.BigDecimal;

public class User {
    private int id;
    private String username;
    private String password;
    private BigDecimal balance;
    private Integer auctionIdBidding; // NULL if not currently in an auction

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    public Integer getAuctionIdBidding() {
        return auctionIdBidding;
    }

    public void setAuctionIdBidding(Integer auctionIdBidding) {
        this.auctionIdBidding = auctionIdBidding;
    }
}
