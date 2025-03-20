#!/usr/bin/env python3

import sys
import time
import aiohttp
import asyncio
import re
import json

API_KEY = "YOUR API KEY"

baileyPort = 12006
bonaPort = 12037
campbellPort = 12068
clarkPort = 12099
jacquezPort = 12130

name_to_port = {
    "Bailey": baileyPort,
    "Bona": bonaPort,
    "Campbell": campbellPort,
    "Clark": clarkPort,
    "Jacquez": jacquezPort
}

port_to_name = {v: k for k, v in name_to_port.items()}

port_to_talks_to = {
    baileyPort: [bonaPort, campbellPort],
    bonaPort: [baileyPort, campbellPort, clarkPort],
    campbellPort: [bonaPort, baileyPort, jacquezPort],
    clarkPort: [bonaPort, jacquezPort],
    jacquezPort: [clarkPort, campbellPort]
}

user_data = {}

url = "https://places.googleapis.com/v1/places:searchNearby"
headers = {
    'Content-Type': 'application/json',
    'X-Goog-Api-Key': API_KEY,
    'X-Goog-FieldMask': "*",
}

async def send_error(writer, msg):
    writer.write(f'? {msg}\n'.encode())
    await writer.drain()
    writer.close()
    await writer.wait_closed()

async def flood_update(client_id, time_diff, location, message_sent_time, server_name):
    message = f'UPDATE {client_id} {time_diff} {location} {message_sent_time} {server_name}'
    for neighbor_port in port_to_talks_to[name_to_port[server_name]]:
        try:
            print("sending message to", port_to_name[neighbor_port])
            reader1, writer1 = await asyncio.open_connection('127.0.0.1', neighbor_port)
            writer1.write(message.encode())
            await writer1.drain()
            writer1.close()
            await writer1.wait_closed()
        except:
            pass

async def handle_requests(reader, writer):
    data = await reader.read(1000)
    message = data.decode().strip()
    print("received message:\n", message, '\n')

    message_args = message.split()
    server_port = writer.get_extra_info("sockname")[1]
    server_name = port_to_name[server_port]

    if not message_args:
        await send_error(writer, message)
        return

    if message_args[0] == "IAMAT":
        if len(message_args) != 4:
            await send_error(writer, message)
            return

        client_id, location, message_sent_time = message_args[1], message_args[2], message_args[3]
        try:
            time_diff = str(time.time() - float(message_sent_time))
        except:
            await send_error(writer, message)
            return

        user_data[client_id] = [time_diff, location, message_sent_time, server_name]
        await flood_update(client_id, time_diff, location, message_sent_time, server_name)

        response = f'AT {server_name} {time_diff} {client_id} {location} {message_sent_time}\n'
        writer.write(response.encode())
        await writer.drain()
        writer.close()
        await writer.wait_closed()

    elif message_args[0] == "WHATSAT":
        if len(message_args) != 4:
            await send_error(writer, message)
            return

        client_id, radius, info_bound = message_args[1], float(message_args[2]) * 1000, int(message_args[3])

        if client_id not in user_data or not (0 <= radius <= 50000) or not (0 <= info_bound <= 20):
            await send_error(writer, message)
            return

        match = re.match(r"([+-][\d.]+)([+-][\d.]+)", user_data[client_id][1])
        if not match:
            await send_error(writer, message)
            return

        latitude, longitude = float(match.group(1)), float(match.group(2))
        data = {
            "maxResultCount": info_bound,
            "locationRestriction": {
                "circle": {
                    "center": {
                        "latitude": latitude,
                        "longitude": longitude
                    },
                    "radius": radius
                }
            }
        }

        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(url, headers=headers, json=data) as response_data:
                    if response_data.status == 200:
                        api_result = await response_data.json()
                        response = f'AT {user_data[client_id][3]} {user_data[client_id][0]} {client_id} {user_data[client_id][1]} {user_data[client_id][2]}\n{json.dumps(api_result, indent=4)}\n'
                        writer.write(response.encode())
                        await writer.drain()
        except:
            await send_error(writer, message)
            return

        writer.close()
        await writer.wait_closed()

    elif message_args[0] == 'UPDATE':
        if len(message_args) != 6:
            await send_error(writer, message)
            return

        client_id, time_diff, location, message_sent_time, sent_server = message_args[1:]

        if (client_id not in user_data) or (float(message_sent_time) > float(user_data[client_id][2])):
            user_data[client_id] = [time_diff, location, message_sent_time, sent_server]
            await flood_update(client_id, time_diff, location, message_sent_time, sent_server)

        writer.close()
        await writer.wait_closed()

    else:
        await send_error(writer, message)

async def main():
    if sys.argv[0].endswith("test.py") and sys.argv[1] in name_to_port:
        server = await asyncio.start_server(handle_requests, '127.0.0.1', name_to_port[sys.argv[1]])
        addrs = ', '.join(str(sock.getsockname()) for sock in server.sockets)
        print(f'Serving on {addrs}')

        async with server:
            await server.serve_forever()
    else:
        print('?', ' '.join(sys.argv))
        exit()

asyncio.run(main())
