import os
import discord
from discord.ext import commands

output_file = os.path.expanduser("~/nothing-but-stats/app/data/roster-log.txt")
TOKEN = os.getenv("DISCORD_BOT_TOKEN")
CHANNEL_ID = 784159736706105395

intents = discord.Intents.default()
intents.messages = True
intents.message_content = True

bot = commands.Bot(command_prefix="!", intents=intents)

@bot.event
async def on_ready():
    print(f"Logged in as {bot.user}")
    channel = bot.get_channel(CHANNEL_ID)

    if channel is None:
        print("Channel not found. Check ID.")
        await bot.close()
        return

    print(f"Fetching messages from channel: {channel.name}")
    messages = []
    async for message in channel.history(limit=None):
        messages.append(f"{message.created_at};FARTS;{message.content.replace('\n', '\\n')}")

    with open(output_file, "w", encoding="utf-8") as file:
        file.write("\n".join(messages))

    print(f"Saved {len(messages)} messages to file.")
    await bot.close()

bot.run(TOKEN)
