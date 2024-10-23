#!/usr/bin/zsh
CMDLINE_TOOLS_VERSION=commandlinetools-linux-11076708_latest
ANDROID_VERSION="34"
NDK_VERSION="26.1.10909125"
ZIP_OUTPUT=commandlinetools.zip

sdkmanager=$ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager
avdmanager="$ANDROID_HOME"/cmdline-tools/latest/bin/avdmanager

cd ~/Downloads/ || exit
wget https://dl.google.com/android/repository/$CMDLINE_TOOLS_VERSION.zip -O $ZIP_OUTPUT

mkdir -p "$ANDROID_HOME"/cmdline-tools
unzip commandlinetools.zip -d "$ANDROID_HOME"/cmdline-tools
mv "$ANDROID_HOME"/cmdline-tools/cmdline-tools "$ANDROID_HOME"/cmdline-tools/latest

yes | sudo -E "$sdkmanager" "build-tools;$ANDROID_VERSION.0.0" "platform-tools" "platforms;android-$ANDROID_VERSION" "emulator" "ndk;$NDK_VERSION"
sudo -E "$sdkmanager" --install "system-images;android-$ANDROID_VERSION;google_apis_playstore;x86_64"

yes | $sdkmanager --licenses

#TODO: template of device
# name=Test_Pixel_2_API_33
# cd "$ANDROID_HOME"/tools/bin || exit
# echo no | avdmanager create avd --force --name $name --abi arm64-v8a --package 'system-images;android-33-ext5;google_apis_playstore;arm64-v8a'
# cat ~/pixel_2_API_33_config >>"$HOME"/.android/avd/$name.avd/config.ini
# cd "$ANDROID_HOME"/emulator || exit
# ./emulator -list-avds
#
rm -rf $ZIP_OUTPUT
