#!/usr/bin/env python3
"""
Post NBN standings, games, and news to Discord when the refresh pipeline runs.
Reads token from DISCORD_BOT_TOKEN. Optional LAST_UPDATED_FILE for date.
"""

import logging
import os
import re
from datetime import datetime

import discord
import feedparser
import pandas as pd
import requests

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Channel IDs (main: standings/games/news; stats: games + react instruction)
CHANNEL_ID = 983834015570227240
STATS_CHANNEL_ID = 717468276137001071
CSV_URL = "https://stats.nbn.today/files/standings.csv"
ESPN_SCHEDULE_URL = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard"
NEWS_FEED_URL = "https://news.nbn.today/feeds/posts/default?alt=rss"
SITE_URL = "https://stats.nbn.today/"

TEAM_EMOJIS = {
    "Boston Celtics": "<:Celtics:664235220756856862>",
    "Brooklyn Nets": "<:Nets:664235220874297385>",
    "New York Knicks": "<:Knicks:664235220966572054>",
    "Philadelphia 76ers": "<:Sixers:664235220962377732>",
    "Toronto Raptors": "<:Raptors:664235220563918871>",
    "Chicago Bulls": "<:Bulls:664235220735623178>",
    "Cleveland Cavaliers": "<:Cavs:664235220672839710>",
    "Detroit Pistons": "<:Pistons:664235220693680204>",
    "Indiana Pacers": "<:Pacers:664235220761051157>",
    "Milwaukee Bucks": "<:Bucks:664235220647673861>",
    "Atlanta Hawks": "<:Hawks:664235220731428874>",
    "Charlotte Hornets": "<:Hornets:664235220882423808>",
    "Miami Heat": "<:Heat:664235220618313732>",
    "Orlando Magic": "<:Magic:664235220937211945>",
    "Washington Wizards": "<:Wizards:664235220853063680>",
    "Denver Nuggets": "<:Nuggets:664235220744273942>",
    "Minnesota Timberwolves": "<:Wolves:664235221087944725>",
    "Oklahoma City Thunder": "<:Thunder:664235220949663744>",
    "Portland Trail Blazers": "<:Blazers:664235220614250519>",
    "Utah Jazz": "<:jazz:664235288796856330>",
    "Golden State Warriors": "<:Warriors:664235220865908737>",
    "Los Angeles Clippers": "<:Clippers:664235220718845952>",
    "LA Clippers": "<:Clippers:664235220718845952>",
    "Los Angeles Lakers": "<:Lakers:664235220928692255>",
    "Phoenix Suns": "<:suns:664235289329401857>",
    "Sacramento Kings": "<:Kings:664235220597473321>",
    "Dallas Mavericks": "<:Mavs2:664235261554589709>",
    "Houston Rockets": "<:rockets:664235262158700554>",
    "Memphis Grizzlies": "<:Grizzlies:664235220358397975>",
    "New Orleans Pelicans": "<:Pelicans:664235220874297344>",
    "San Antonio Spurs": "<:Spurs:664235220953727044>",
}


def fetch_last_updated_date():
    """Read last updated from file if LAST_UPDATED_FILE set, else scrape site."""
    path = os.environ.get("LAST_UPDATED_FILE")
    if path and os.path.isfile(path):
        try:
            with open(path) as f:
                return f.read().strip() or None
        except OSError as e:
            logger.warning("Could not read LAST_UPDATED_FILE: %s", e)
    try:
        response = requests.get(SITE_URL, timeout=10)
        if response.status_code == 200:
            match = re.search(r"Last updated:\s*([\d-]+)", response.text)
            if match:
                return match.group(1)
    except Exception as e:
        logger.warning("Could not fetch last updated from site: %s", e)
    return None


def scrape_nbn_standings():
    try:
        last_updated_date = fetch_last_updated_date()
        df = pd.read_csv(CSV_URL)
        east_df = df[df["SEED"].str.contains("East")].copy()
        west_df = df[df["SEED"].str.contains("West")].copy()
        east_df["SEED"] = east_df["SEED"].str.replace("East-", "", regex=False)
        west_df["SEED"] = west_df["SEED"].str.replace("West-", "", regex=False)
        standings_text = "🏀 **NBN Regular Season Standings** 🏀\n"
        if last_updated_date:
            standings_text += f"📅 *Last updated on: {last_updated_date}*\n\n"
        else:
            standings_text += "\n"
        dashes_line = "-" * 23
        standings_text += "**Eastern Conference**\n```\n"
        standings_text += f"{'SEED':<6}{'TEAM':<6}{'W':<3}{'L':<3}{'PCT':<5}\n{dashes_line}\n"
        for _, row in east_df.iterrows():
            team = row["TEAM"]
            emoji = TEAM_EMOJIS.get(team, team)
            team_display = f"{emoji} {team}" if emoji != team else team
            standings_text += (
                f"{str(row['SEED']):<6}"
                f"{team_display:<6}"
                f"{str(row['W']):<3}"
                f"{str(row['L']):<3}"
                f"{str(row['PCT']):<5}\n"
            )
        standings_text += "```\n**Western Conference**\n```\n"
        standings_text += f"{'SEED':<6}{'TEAM':<6}{'W':<3}{'L':<3}{'PCT':<5}\n{dashes_line}\n"
        for _, row in west_df.iterrows():
            team = row["TEAM"]
            emoji = TEAM_EMOJIS.get(team, team)
            team_display = f"{emoji} {team}" if emoji != team else team
            standings_text += (
                f"{str(row['SEED']):<6}"
                f"{team_display:<6}"
                f"{str(row['W']):<3}"
                f"{str(row['L']):<3}"
                f"{str(row['PCT']):<5}\n"
            )
        standings_text += "```\n\n📊 *Updated from [NBN Stats](https://stats.nbn.today/)*"
        return standings_text[:2000]
    except Exception as e:
        logger.error("Error in scrape_nbn_standings: %s", e)
        return f"Error fetching standings: {e}"


def scrape_espn_games():
    try:
        response = requests.get(ESPN_SCHEDULE_URL, timeout=10)
        if response.status_code != 200:
            return "Error fetching ESPN schedule."
        data = response.json()
        events = data.get("events", [])
        if not events:
            return "No NBN games scheduled for today."
        games_text = "**🏀 NBN Games Today 🏀**\n"
        for game in events:
            competitors = game["competitions"][0]["competitors"]
            home_team = competitors[0]["team"]["displayName"]
            away_team = competitors[1]["team"]["displayName"]
            home_emoji = TEAM_EMOJIS.get(home_team, home_team)
            away_emoji = TEAM_EMOJIS.get(away_team, away_team)
            games_text += f"{away_emoji} {away_team}  @  {home_emoji} {home_team}\n"
        games_text += "\n📅 *Game schedule updated daily from ESPN.*"
        return games_text[:2000]
    except Exception as e:
        logger.error("Error in scrape_espn_games: %s", e)
        return f"Error fetching NBA games: {e}"


def fetch_nbn_news():
    try:
        feed = feedparser.parse(NEWS_FEED_URL)
        if not feed.entries:
            return None
        news_text = "**📰 Latest NBN News Articles:**\n\n"
        for entry in feed.entries[:5]:
            if "published_parsed" in entry and entry.published_parsed:
                dt = datetime(*entry.published_parsed[:6])
                date_str = f"{dt.month}/{dt.day}/{dt.year % 100:02}"
            else:
                date_str = "Unknown date"
            news_text += f"🔹 [**{entry.title} ({date_str})**]({entry.link})\n"
        return news_text[:2000]
    except Exception as e:
        logger.error("Error in fetch_nbn_news: %s", e)
        return None


def main():
    token = os.environ.get("DISCORD_BOT_TOKEN")
    if not token:
        logger.error("DISCORD_BOT_TOKEN is not set. Skipping Discord post.")
        return

    intents = discord.Intents.default()
    intents.guilds = True
    client = discord.Client(intents=intents)

    @client.event
    async def on_ready():
        main_channel = client.get_channel(CHANNEL_ID)
        stats_channel = client.get_channel(STATS_CHANNEL_ID)
        if not main_channel or not stats_channel:
            logger.error("Invalid channel ID(s); could not get channels.")
            await client.close()
            return
        standings = scrape_nbn_standings()
        games = scrape_espn_games()
        news = fetch_nbn_news()
        await main_channel.send(standings)
        await main_channel.send(games)
        if news:
            await main_channel.send(news)
        current_date = datetime.now().strftime("%B %d, %Y")
        stats_games_message = (
            f"📅 **{current_date}**\n{games}\n"
            "📊 **React to this message with the team's emoji once the stats are entered. "
            "(Use the same emoji as the schedule)**"
        )
        await stats_channel.send(stats_games_message)
        logger.info("Discord post completed.")
        await client.close()

    client.run(token)


if __name__ == "__main__":
    main()
