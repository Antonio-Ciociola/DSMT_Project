package com.auction.model;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * Database connection pool manager using HikariCP.
 * Manages connections to the MySQL database with environment-based configuration.
 */
public class DatabaseConnection {
    private static HikariDataSource dataSource;

    static {
        try {
            // Load MySQL JDBC Driver
            Class.forName("com.mysql.cj.jdbc.Driver");
        } catch (ClassNotFoundException e) {
            throw new ExceptionInInitializerError("Failed to load MySQL JDBC Driver: " + e.getMessage());
        }
        
        // Get environment variables with defaults for local development
        String dbUrl = System.getenv("DB_URL");
        String dbUser = System.getenv("DB_USER");
        String dbPassword = System.getenv("DB_PASSWORD");
        
        System.out.println("DatabaseConnection initializing...");
        System.out.println("DB_URL: " + dbUrl);
        System.out.println("DB_USER: " + dbUser);
        System.out.println("DB_PASSWORD: " + (dbPassword != null ? "***" : "null"));
        
        if (dbUrl == null || dbUser == null || dbPassword == null) {
            throw new ExceptionInInitializerError(
                "Database configuration missing. Required: DB_URL, DB_USER, DB_PASSWORD"
            );
        }
        
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl(dbUrl);
        config.setUsername(dbUser);
        config.setPassword(dbPassword);
        config.setMaximumPoolSize(10);
        config.setMinimumIdle(5);
        config.setConnectionTimeout(20000);
        config.setIdleTimeout(300000);
        
        try {
            dataSource = new HikariDataSource(config);
            System.out.println("DatabaseConnection initialized successfully");
        } catch (Exception e) {
            System.err.println("Failed to initialize HikariCP: " + e.getMessage());
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Gets a connection from the connection pool.
     * @return a Connection object from the pool
     * @throws SQLException if a database access error occurs
     */
    public static Connection getConnection() throws SQLException {
        return dataSource.getConnection();
    }

    /**
     * Closes the connection pool.
     * Should be called when the application is shutting down.
     */
    public static void closePool() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
        }
    }
}
