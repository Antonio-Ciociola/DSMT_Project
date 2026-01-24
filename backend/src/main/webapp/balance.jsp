<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Manage Balance</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background: #f5f5f5; color: #333; }
        .container { max-width: 600px; margin: 40px auto; padding: 0 20px; }
        .card { background: white; padding: 30px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        h1 { margin-bottom: 20px; color: #667eea; font-size: 24px; }
        .balance { font-size: 32px; font-weight: bold; margin-bottom: 20px; color: #333; }
        .label { font-size: 14px; color: #666; margin-bottom: 8px; }
        input[type="number"] { width: 100%; padding: 12px; border: 1px solid #ddd; border-radius: 6px; margin-bottom: 16px; font-size: 16px; }
        button { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none; padding: 12px 20px; border-radius: 6px; cursor: pointer; font-size: 15px; font-weight: 600; width: 100%; }
        button:hover { box-shadow: 0 3px 10px rgba(102,126,234,0.3); }
        .message { margin-bottom: 16px; padding: 12px; border-radius: 6px; font-size: 14px; }
        .error { background: #fdecea; color: #b71c1c; border: 1px solid #f5c6cb; }
        .success { background: #e6ffed; color: #1b5e20; border: 1px solid #c3e6cb; }
        .back { display: inline-block; margin-top: 16px; color: #667eea; text-decoration: none; font-weight: 600; }
    </style>
</head>
<body>
    <div class="container">
        <div class="card">
            <h1>Manage Balance</h1>

            <c:if test="${not empty errorMessage}">
                <div class="message error">${errorMessage}</div>
            </c:if>
            <c:if test="${not empty successMessage}">
                <div class="message success">${successMessage}</div>
            </c:if>

            <div class="balance">$${balance}</div>

            <form method="POST" action="${pageContext.request.contextPath}/balance">
                <div class="label">Amount to add</div>
                <input type="number" name="amount" step="0.01" min="0.01" placeholder="Enter amount" required>
                <button type="submit">Add Funds</button>
            </form>

            <a class="back" href="${pageContext.request.contextPath}/home">← Back to Home</a>
        </div>
    </div>
</body>
</html>
