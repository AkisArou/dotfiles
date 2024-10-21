import json
import os

# Define the file paths
file1_path = '/usr/lib64/librewolf/distribution/policies.json'
file2_path = os.path.expanduser('~/dotfiles/librewolf/policies.json')  # Expand the tilde
output_path = '/usr/lib64/librewolf/distribution/policies.json'  # Output path set to original

# Load the JSON files
with open(file1_path, 'r') as file1:
    data1 = json.load(file1)

with open(file2_path, 'r') as file2:
    data2 = json.load(file2)

# Merge the policies
merged_data = {
    "__COMMENT__ More Information": data1.get("__COMMENT__ More Information"),
    "policies": data1["policies"]  # Start with the policies from the first file
}

# Update the Install extensions while ensuring uniqueness
install_extensions = set(data1["policies"].get("Extensions", {}).get("Install", [])) | set(data2["policies"].get("Extensions", {}).get("Install", []))
merged_data["policies"]["Extensions"] = {
    "Install": list(install_extensions),
    "Uninstall": data1["policies"]["Extensions"].get("Uninstall", [])
}

# Add the other policy properties from the first file
for key, value in data1["policies"].items():
    if key not in merged_data["policies"]:
        merged_data["policies"][key] = value

# Write the merged data to the output file
with open(output_path, 'w') as output_file:
    json.dump(merged_data, output_file, indent=4)

print(f"Merged librewolf policies and written to {output_path}.")

