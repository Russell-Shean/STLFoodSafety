import requests
import zstandard as zstd
import io

# Base URL
url = "https://pressagent.envisionconnect.com/insp.phtml"

# Query parameters
params = {
    "agency": "stl",
    "violsortfield": "TB_CORE_INSPECTION_VIOL.VIOLATION_CODE",
    "record_id": "PR0012616"
}

# Headers to mimic a browser
headers = {
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Encoding": "gzip, deflate, br, zstd",
    "Accept-Language": "en-US,en;q=0.9",
    "Connection": "keep-alive",
    "DNT": "1",
    "Host": "pressagent.envisionconnect.com",
    "Upgrade-Insecure-Requests": "1",
    "User-Agent": "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:148.0) Gecko/20100101 Firefox/148.0"
}

# Make the GET request
response = requests.get(url, params=params, headers=headers)

# Check the response
if response.status_code == 200:
    print("Request successful!")
    
    # Check for zstd compression
    if response.headers.get("Content-Encoding") == "zstd":
        dctx = zstd.ZstdDecompressor()
        
        with dctx.stream_reader(io.BytesIO(response.content)) as reader:
            text = reader.read().decode("utf-8")
        
    else:
        text = response.text
        
        
    print(text)



    


    #print(f"text: {response.text}")  # or process as needed
    print(f"json: {response.json}")
    print(f"headers: {response.headers}")
else:
    print(f"Request failed with status code {response.status_code}")