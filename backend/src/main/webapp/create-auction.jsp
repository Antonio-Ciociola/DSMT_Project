<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%
    String errorMessage = (String) request.getAttribute("errorMessage");
%>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Auction System - Create Auction</title>

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
        }

        .header-content {
            max-width: 1200px;
            margin: auto;
            padding: 0 20px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        h1 { font-size: 28px; }

        .back-link {
            background: rgba(255,255,255,0.2);
            color: white;
            padding: 8px 16px;
            border-radius: 5px;
            text-decoration: none;
        }

        .container {
            max-width: 600px;
            margin: 40px auto;
            padding: 0 20px;
        }

        .form-card {
            background: white;
            padding: 40px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }

        h2 {
            color: #667eea;
            margin-bottom: 30px;
        }

        .error-message {
            background: #fee;
            border-left: 4px solid #e74c3c;
            color: #c0392b;
            padding: 12px;
            margin-bottom: 20px;
            border-radius: 4px;
        }

        .form-group {
            margin-bottom: 20px;
        }

        label {
            display: block;
            margin-bottom: 8px;
            font-weight: 500;
            font-size: 14px;
        }

        input, textarea {
            width: 100%;
            padding: 12px;
            border: 1px solid #ddd;
            border-radius: 5px;
            font-size: 14px;
        }

        textarea {
            resize: vertical;
            min-height: 120px;
        }

        .time-inputs {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 10px;
        }

        .button-group {
            display: flex;
            gap: 10px;
            margin-top: 30px;
        }

        button {
            flex: 1;
            padding: 12px;
            border: none;
            border-radius: 5px;
            font-size: 16px;
            background: linear-gradient(135deg, #667eea, #764ba2);
            color: white;
            cursor: pointer;
        }

        .cancel-btn {
            flex: 1;
            padding: 12px;
            background: #e0e0e0;
            text-decoration: none;
            text-align: center;
            border-radius: 5px;
            color: #333;
        }
    </style>
</head>

<body>

<header>
    <div class="header-content">
        <h1>🏆 Create Auction</h1>
        <a href="${pageContext.request.contextPath}/home" class="back-link">← Back</a>
    </div>
</header>

<div class="container">
    <div class="form-card">
        <h2>Create New Auction</h2>

        <% if (errorMessage != null) { %>
            <div class="error-message"><%= errorMessage %></div>
        <% } %>

        <form method="POST" action="${pageContext.request.contextPath}/create-auction" id="auctionForm">

            <div class="form-group">
                <label>Auction Title</label>
                <input type="text" name="title" required>
            </div>

            <div class="form-group">
                <label>Description</label>
                <textarea name="description"></textarea>
            </div>

            <div class="form-group">
                <label>Starting Price ($)</label>
                <input type="number" name="startingPrice" step="0.01" min="0" required>
            </div>

            <div class="form-group">
                <label>Minimum Bid Increment ($)</label>
                <input type="number" name="minBidIncrement" step="0.01" min="0.01" required>
            </div>

            <div class="form-group">
                <label>Starting Duration:</label>
                <div class="time-inputs">
                    <div>
                        <label style="font-size: 12px;">Hours</label>
                        <input type="number" name="initialWaitHours" min="0" value="0">
                    </div>
                    <div>
                        <label style="font-size: 12px;">Minutes</label>
                        <input type="number" name="initialWaitMinutes" min="0" max="59" value="5">
                    </div>
                    <div>
                        <label style="font-size: 12px;">Seconds</label>
                        <input type="number" name="initialWaitSeconds" min="0" max="59" value="0">
                    </div>
                </div>
            </div>

            <div class="form-group">
                <label>Bid Time Increment:</label>
                <div class="time-inputs">
                    <div>
                        <label style="font-size: 12px;">Hours</label>
                        <input type="number" name="bidTimeHours" min="0" value="0">
                    </div>
                    <div>
                        <label style="font-size: 12px;">Minutes</label>
                        <input type="number" name="bidTimeMinutes" min="0" max="59" value="0">
                    </div>
                    <div>
                        <label style="font-size: 12px;">Seconds</label>
                        <input type="number" name="bidTimeSeconds" min="0" max="59" value="30">
                    </div>
                </div>
            </div>

            <div class="form-group">
                <label>
                    <input type="checkbox"
                           id="startNow"
                           name="startNow"
                           value="true"
                           style="width:auto; margin-right:8px;">
                    <strong style="color:#667eea;">Start Now</strong>
                </label>
            </div>

            <div class="form-group">
                <label>Start Date</label>
                <input type="date" id="startDate" name="startDateLocal">
            </div>

            <div class="form-group">
                <label>Start Time</label>
                <input type="time" id="startTime" name="startTimeLocal">
            </div>
            
            <!-- Hidden fields for UTC conversion -->
            <input type="hidden" id="startDateUTC" name="startDate">
            <input type="hidden" id="startTimeUTC" name="startTime">
            <input type="hidden" id="timezoneOffset" name="timezoneOffset" value="">

            <div class="button-group">
                <button type="submit">Create Auction</button>
                <a href="${pageContext.request.contextPath}/home" class="cancel-btn">Cancel</a>
            </div>

        </form>
    </div>
</div>

<script>
document.addEventListener('DOMContentLoaded', () => {
    const form = document.getElementById('auctionForm');
    const startDate = document.getElementById('startDate');
    const startTime = document.getElementById('startTime');
    const startDateUTC = document.getElementById('startDateUTC');
    const startTimeUTC = document.getElementById('startTimeUTC');
    const startNow = document.getElementById('startNow');
    const timezoneOffset = document.getElementById('timezoneOffset');
    
    // Set timezone offset in minutes (negative for UTC+, positive for UTC-)
    timezoneOffset.value = new Date().getTimezoneOffset();

    function now() {
        const d = new Date();
        return {
            date: d.toISOString().slice(0, 10),
            time: d.toTimeString().slice(0, 5)
        };
    }

    const today = now();
    startDate.value = today.date;
    startDate.min = today.date;

    startNow.addEventListener('change', () => {
        if (startNow.checked) {
            const current = now();
            startDate.value = current.date;
            startTime.value = current.time;

            startDate.readOnly = true;
            startTime.readOnly = true;
            startDate.style.pointerEvents = 'none';
            startTime.style.pointerEvents = 'none';
            startDate.style.background = '#e9ecef';
            startTime.style.background = '#e9ecef';
        } else {
            startDate.readOnly = false;
            startTime.readOnly = false;
            startDate.style.pointerEvents = '';
            startTime.style.pointerEvents = '';
            startDate.style.background = '';
            startTime.style.background = '';
        }
    });
    
    // Convert local datetime to UTC before form submission
    form.addEventListener('submit', (e) => {
        if (startNow.checked) {
            // If "Start Now" is checked, use current UTC time
            const nowUTC = new Date();
            startDateUTC.value = nowUTC.toISOString().slice(0, 10);
            startTimeUTC.value = nowUTC.toISOString().slice(11, 16);
        } else if (startDate.value && startTime.value) {
            // Convert user's local datetime to UTC
            const localDateTimeStr = startDate.value + 'T' + startTime.value;
            const localDateTime = new Date(localDateTimeStr);
            
            // Convert to UTC
            const utcDate = new Date(localDateTime.getTime());
            startDateUTC.value = utcDate.toISOString().slice(0, 10);
            startTimeUTC.value = utcDate.toISOString().slice(11, 16);
        }
    });
});
</script>

</body>
</html>
