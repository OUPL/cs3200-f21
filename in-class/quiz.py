import urllib.request, json

def gen_sample(bits):
    i = 0
    while i < len(bits) - 3:
        if bits[i:i+2] == "11":
            i += 2
        else:
            return bits[i:i+3] == "000"

with urllib.request.urlopen("https://beacon.nist.gov/beacon/2.0/pulse/last") as url:
    data = json.loads(url.read().decode())

    # Convert hex string to bit string.
    bits = bin(int(data["pulse"]["outputValue"], 16))[2:]
    
    # Add leading zero bits omitted by the conversion.
    bits = "0" * (512 - len(bits)) + bits

    print(bits)
    print("QUIZ!" if gen_sample(bits) else "NO QUIZ!")
