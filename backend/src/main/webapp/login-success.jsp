<%@ page contentType="text/html;charset=UTF-8" %>
<!DOCTYPE html>
<html>
<head>
    <title>Login Successful</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
            margin: 0;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        .container {
            text-align: center;
            background: white;
            padding: 40px;
            border-radius: 8px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #333;
            margin: 0 0 10px 0;
        }
        p {
            color: #666;
            margin: 10px 0;
        }
        .loading {
            font-size: 18px;
            color: #667eea;
            font-weight: bold;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Welcome, ${username}!</h1>
        <p class="loading">Storing authentication token...</p>
        <p>Redirecting to home page...</p>
    </div>

    <script>
        // Store JWT token in browser's localStorage
        const token = '${jwtToken}';
        if (token) {
            localStorage.setItem('jwtToken', token);
            console.log('JWT token stored in localStorage');
        }
        
        // Redirect to home after a short delay
        setTimeout(() => {
            window.location.href = '<%= request.getContextPath() %>/home';
        }, 500);
    </script>
</body>
</html>
