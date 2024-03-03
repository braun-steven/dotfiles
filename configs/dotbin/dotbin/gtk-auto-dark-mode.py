#!/usr/bin/env python3
"""
Enhanced script to automatically change the GNOME theme between light and dark mode
based on the sunrise and sunset times. Now includes improved error handling, logging,
and allows for manual location specification.
"""

import argparse
import logging
import requests
import subprocess
from datetime import datetime
import pytz
from astral import LocationInfo
from astral.sun import sun

# Setup basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def get_current_location_info():
    try:
        response = requests.get("https://ipinfo.io/json")
        response.raise_for_status()  # Raises an HTTPError if the response was an error
        data = response.json()
        loc = data["loc"].split(",")
        return data["city"], float(loc[0]), float(loc[1]), data["timezone"]
    except requests.RequestException as e:
        logging.error("Failed to fetch current location: %s", e)
        raise SystemExit("Could not determine current location. Please specify location manually.")

def get_sunrise_sunset(latitude, longitude, timezone_str):
    try:
        timezone = pytz.timezone(timezone_str)
        city = LocationInfo(latitude=latitude, longitude=longitude, timezone=timezone_str)
        s = sun(city.observer, date=datetime.now(timezone), tzinfo=timezone)
        return s["sunrise"], s["sunset"]
    except Exception as e:
        logging.error("Error calculating sunrise/sunset: %s", e)
        raise

def run(cmd):
    try:
        subprocess.run(cmd.split(), check=True)
    except subprocess.CalledProcessError as e:
        logging.error("Command failed: %s", e)
        raise

def set_theme(mode):
    try:
        if mode == "dark":
            run("gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark")
            run("gsettings set org.gnome.desktop.interface color-scheme prefer-dark")
        else:
            run("gsettings set org.gnome.desktop.interface gtk-theme Adwaita")
            run("gsettings set org.gnome.desktop.interface color-scheme prefer-light")
    except Exception as e:
        logging.error("Failed to set theme: %s", e)
        raise

def main():
    parser = argparse.ArgumentParser(description="Automatically set GNOME theme based on local sunrise and sunset times.")
    parser.add_argument("--mode", type=str, choices=["light", "dark", "auto"], default="auto", help="Theme mode: light, dark, or auto (default).")
    parser.add_argument("--location", nargs=3, metavar=('LATITUDE', 'LONGITUDE', 'TIMEZONE'), help="Manually specify location as latitude longitude timezone.")
    args = parser.parse_args()

    if args.location:
        latitude, longitude, timezone_str = map(float, args.location[:2]) + [args.location[2]]
    else:
        city_name, latitude, longitude, timezone_str = get_current_location_info()

    if args.mode in ["light", "dark"]:
        set_theme(args.mode)
    else:
        timezone = pytz.timezone(timezone_str)
        sunrise, sunset = get_sunrise_sunset(latitude, longitude, timezone_str)
        now = datetime.now(timezone)

        current_time = now.time()
        sunrise_time = sunrise.time()
        sunset_time = sunset.time()

        if sunrise_time <= current_time <= sunset_time:
            set_theme("light")
        else:
            set_theme("dark")

if __name__ == "__main__":
    main()
