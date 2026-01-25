import argparse
import sys
import requests

# Simple CLI to call the finish-auction endpoint.
# The endpoint is expected to be publicly accessible (no auth required).


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Finish an auction via HTTP POST")
    parser.add_argument("auction_id", type=int, help="ID of the auction to finish")
    parser.add_argument("winner_id", type=int, help="ID of the winning user")
    parser.add_argument("final_price", type=float, help="Final price paid by the winner")
    parser.add_argument(
        "--base-url",
        default="http://localhost:8080",
        help="Base URL of the application (without trailing slash)",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    url = f"{args.base_url}/api/finish-auction"
    payload = {
        "auctionId": str(args.auction_id),
        "winnerId": str(args.winner_id),
        "finalPrice": str(args.final_price),
    }

    try:
        resp = requests.post(url, data=payload, timeout=10)
    except requests.RequestException as exc:
        print(f"Request failed: {exc}")
        return 1

    print(f"Status: {resp.status_code}")
    # Try to print JSON if available, else raw text
    try:
        print("Body:", resp.json())
    except ValueError:
        print("Body:", resp.text)

    return 0 if resp.ok else 1


if __name__ == "__main__":
    sys.exit(main())
