
from fastapi import FastAPI
from geopy.distance import geodesic
import requests
import json
import numpy as np
from python_tsp.exact import solve_tsp_dynamic_programming
from dotenv import load_dotenv
import os

load_dotenv()

### ToDo List
### Add Key to Personal API
### Build a Front End
### Put Secrets in Json later

city_history = []
coordinate_history = []

# Initialize the FastAPI app
app = FastAPI()

# Maintain a history of cities and their coordinates
city_history = []
coordinate_history = []


# Function to fetch city information from the API
def fetch_city_info(city):
    api_url = f'https://api.api-ninjas.com/v1/city?name={city}'
    response = requests.get(api_url, headers={'X-Api-Key': os.getenv('API_KEY_CITY')})
    response_data = response.json()
    if response_data:
        return response_data[0]
    return None




# Endpoint to check connection
@app.get('/')
def check_connection():
    return {"message": "Hello and welcome to the distance city"}


@app.post('/reset')
def reset():
    city_history = []
    coordinate_history = []
    return {"message": "All stored data has been reset"}

@app.post('/get_distance')
def get_distance(request: dict):
    city_1 = request['city_1']
    city_2 = request['city_2']

    city_info_1 = fetch_city_info(city_1)
    city_info_2 = fetch_city_info(city_2)

    if not city_info_1 and not city_info_2:
        return {"Connection": 'ok', 'type': 'error', 'message': "One or more invalid Cities"}

    city_coordinates_1 = [city_info_1['latitude'], city_info_1['longitude']]
    city_coordinates_2 = [city_info_2['latitude'], city_info_2['longitude']]

    distance = geodesic(city_coordinates_1, city_coordinates_2).km

    return {"Connection": 'ok', 'type': 'success', 'message': "Distance Calculated",
            'city_coordinates(lat/long)': [city_coordinates_1, city_coordinates_2], 'distance': distance}

# Endpoint to add a city to history
@app.post('/add_city')
def add_city(request: dict):

    city = request['city']

    print(city)

    # Fetch information about the requested city
    city_info = fetch_city_info(city)

    print(city_info)

    # If city info is not found, raise an HTTPException
    if not city_info:
        return {"Connection": 'ok', 'type': 'error', 'message': "Invalid City"}

    # If the city is already in history, remove it before adding again
    if city in city_history:
        index = city_history.index(city)
        city_history.pop(index)
        coordinate_history.pop(index)

    # Add the city info and coordinates to history

    city_coordinates = [city_info['latitude'], city_info['longitude']]

    coordinate_history.append(city_coordinates)
    city_history.append(city)

    # Calculate and return distance if there are at least 2 cities in history
    if len(city_history) > 1:
        distance = geodesic(coordinate_history[-2], coordinate_history[-1]).km
        return {"Connection": 'ok', 'type': 'success', 'message': "City Added", 'city_coordinates(lat/long)': city_coordinates, 'distance_from_last_request': distance}
    else:
        return {"Connection": 'ok', 'type': 'success', 'message': "City Added", 'city_coordinates(lat/long)': city_coordinates, 'distance_from_last_request': 'Only one city requested'}


# Endpoint to remove a city from history
@app.post('/remove_city')
def remove_city(request: dict):
    city_to_remove = request["city"]
    if city_to_remove in city_history:
        index = city_history.index(city_to_remove)
        city_history.pop(index)
        coordinate_history.pop(index)
        return {"Connection": 'ok', 'type': 'success', 'message': "Requested City Removed"}
    else:
        return {"Connection": 'ok', 'type': 'error', 'message': "Requested City not previously added"}


# Endpoint to get the city history
@app.get('/get_city_hist')
def get_city_hist():
    return {"Connection": 'ok', 'type': 'success', 'message': "Requested history returned", 'city_history': city_history}


# Endpoint to get the city history
@app.get('/get_tsp')
def get_tsp():
    n = len(coordinate_history)
    dist_matrix = np.zeros((n, n))
    for x in range(n):
        for y in range(x, n):
            dist_matrix[x, y] = geodesic(coordinate_history[x], coordinate_history[y]).km
            dist_matrix[y, x] = dist_matrix[x, y]
    permutation, distance = solve_tsp_dynamic_programming(dist_matrix)

    name_permutation = [city_history[i] for i in permutation]

    return {"Connection": 'ok', 'message': "Best Route Found", 'Route_nr': permutation,'Route_name': name_permutation, 'Distance': distance}

### uvicorn main:app --reload

###p