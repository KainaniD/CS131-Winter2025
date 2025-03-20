#!/usr/bin/env python3

import sys
import time
import aiohttp
import asyncio
import requests
import re
import json

API_KEY="YOUR API KEY"

baileyPort = 12006
bonaPort = 12037
campbellPort = 12068
clarkPort = 12099
jaquezPort = 12130

name_to_port = {
    "Bailey":   baileyPort, #port1
    "Bona":     bonaPort, #port2
    "Campbell": campbellPort, #port3
    "Clark":    clarkPort, #port4
    "Jaquez":  jaquezPort #port5
}

port_to_name = {
    baileyPort: "Bailey", #port1
    bonaPort: "Bona", #port2
    campbellPort: "Campbell", #port3
    clarkPort: "Clark", #port4
    jaquezPort: "Jaquez" #port5
}

port_to_talks_to = {
    baileyPort: [bonaPort, campbellPort], # Bailey -> Bona / Campbell
    bonaPort: [baileyPort, campbellPort, clarkPort], # Bona -> Bailey / Campbell / Clark
    campbellPort: [bonaPort, baileyPort, jacquezPort], # Campbell -> Bona / Bailey / Jaquez
    clarkPort: [bonaPort, jacquezPort], # Clark -> Bona / Jaquez
    jaquezPort: [clarkPort, campbellPort] # Jaquez -> Clark / Campbell
}

user_data = {}

if len(sys.argv) != 2:
    #Invalid Arg count
    print('?', ' '.join(sys.argv))
    exit()

url="https://places.googleapis.com/v1/places:searchNearby"

headers = {
	'Content-Type': 'application/json',
	'X-Goog-Api-Key': API_KEY,
	'X-Goog-FieldMask': "*",
}

async def flood_update(message, server_name):
    #message = <client_id> <time_diff> <location> <message_sent_time> <message_owner> <sent_from>*
    propogate_message = "UPDATE " + " ".join(message) + '\n'
    #server name is the same of the current server

    # print("PROPOGATING MESSAGE:", propogate_message)

    sent_from = set(message[5:])

    # print(sent_from)

    #update and propogate
    for neighbor_port in port_to_talks_to[name_to_port[server_name]]:
        if not str(neighbor_port) in sent_from:
            try:
                print("- sending update to", port_to_name[neighbor_port])
                reader, writer = await asyncio.open_connection('127.0.0.1', neighbor_port)
                writer.write(propogate_message.encode())
                await writer.drain()
                await close_writer(writer)
            except:
                #print("----- failed sending to", port_to_name[neighbor_port])
                pass

async def handle_error(writer, message):
    writer.write(f'? {message}'.encode())
    await writer.drain()
    await close_writer(writer)

async def close_writer(writer):
    writer.close()
    await writer.wait_closed()

async def handle_requests(reader, writer):
    data = await reader.readline()
    message = data.decode()

    message = message.strip()
    message_args = message.split()
    

    server_name = port_to_name[writer.get_extra_info("sockname")[1]]
    
    if len(message_args) == 0:
        await handle_error(writer, message)
        return

    if message_args[0] == "IAMAT" and len(message_args) == 4:

        # message = IAMAT <client_id> <location> <message_sent_time>
        client_id = message_args[1]
        location = message_args[2]
        message_sent_time = message_args[3]

        time_diff = time.time() - float(message_sent_time)
        time_sign = ''
        if time_diff >= 0:
            time_sign = '+'
        time_diff = time_sign + str(time_diff)

        client_data_array = [client_id, time_diff, location, message_sent_time, server_name, str(name_to_port[server_name])]

        if not client_id in user_data.keys():
            print("accept update")
            user_data[client_id] = client_data_array
            await flood_update(client_data_array, server_name)
            response = f"AT {server_name} {time_diff} {client_id} {location} {message_sent_time}\n"
            writer.write(response.encode())
            await writer.drain() 

        elif float(message_sent_time) > float(user_data[client_id][3]):
            print("accept update")
            user_data[client_id] = client_data_array
            await flood_update(client_data_array, server_name)
            response = f"AT {server_name} {time_diff} {client_id} {location} {message_sent_time}\n"
            writer.write(response.encode())
            await writer.drain() 
        else:
            # old message sent
            print("reject update")


    elif message_args[0] == "WHATSAT" and len(message_args) == 4:
        # message = WHATSAT <client_id> <radius> <information_bound>

        # print("recieved WHATSAT from client")

        client_id = message_args[1]

        radius = float(message_args[2]) * 1000
        information_bound = int(message_args[3])

        if client_id in user_data.keys() and (radius <= 50000 and radius >= 0) and (information_bound <= 20 and information_bound >= 0):
            client_data = user_data[client_id]
            match = re.match(r"([+-][\d.]+)([+-][\d.]+)", user_data[client_id][2])
            if not match:
                await handle_error(writer, message)
                return

            latitude = float(match.group(1))
            longitude = float(match.group(2))

            data = {"locationRestriction": {
                    "circle": {
                        "center": {
                            "latitude": latitude,
                            "longitude": longitude},
                        "radius": radius}}}

            async with aiohttp.ClientSession() as session:
                async with session.post(url, headers=headers, json=data) as response:
                    if response.status == 200:
                        #client data array = <client_id> <time_diff> <location> <message_sent_time> <server_name>
                        response_data = await response.json()
                        response_data = response_data.get("results", [])[:information_bound]
                        response_message = f"AT {client_data[4]} {client_data[1]} {client_id} {client_data[2]} {client_data[3]}\n{json.dumps(response_data)}\n"
                        writer.write(response_message.encode())
                        await writer.drain()

        else:
            #invalid WHATSAT message
            await handle_error(writer, message)
            return

    elif message_args[0] == 'UPDATE' and len(message_args) >= 6:
        # message = UPDATE <client_id> <time_diff> <location> <message_sent_time> <message_owner> *<sent_from>
        message_args.append(str(name_to_port[server_name]))
        client_id = message_args[1]
        client_data_array = message_args[1:]
        message_sent_time = message_args[4]


        if not client_id in user_data.keys():
            print("accept update")
            user_data[client_id] = client_data_array
            await flood_update(client_data_array, server_name)
        elif float(message_sent_time) > float(user_data[client_id][3]):
            print("accept update")
            user_data[client_id] = client_data_array
            await flood_update(client_data_array, server_name)
        else:
            # old message sent
            print("reject update")
    else:
        # invalid call arguments
        await handle_error(writer, message)
        return
    await close_writer(writer)



async def main():
    if sys.argv[1] in name_to_port.keys():
        server = await asyncio.start_server(handle_requests, '127.0.0.1', name_to_port[sys.argv[1]])
        addrs = ', '.join(str(sock.getsockname()) for sock in server.sockets)
        print(f'Serving on {addrs}')

        async with server:
            await server.serve_forever()
    else:
        #Invalid Server Name
        print('?', ' '.join(sys.argv))
        exit()

asyncio.run(main())
