while true; do
	read -p "DO YOU WANT TO START THE INSTALLATION NOW? (Yy/Nn): " yn
	case $yn in
	[Yy]*)
		echo "Installation started."
		break
		;;
	[Nn]*)
		exit
		break
		;;
	*) echo "Please answer yes or no." ;;
	esac
done
echo ""

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
echo ""
echo "-> Install main packages"

packagesPacman=(
	"gnome"
	"gnome-tweaks"
)

# ------------------------------------------------------
# Install required packages
# ------------------------------------------------------
_installPackagesPacman "${packagesPacman[@]}"

echo "DONE! Please reboot your system!"
