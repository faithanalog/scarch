# Scarch - A SoundCloud Archiver

Scarch is a wrapper around youtube-dl which can archive content from SoundCloud
faster and more conveniently than using youtube-dl by itself.

## Features

Scarch can...

- Download multiple files in parallel.
  - This helps to alleviate the considerable amount of time
    youtube.dl spends retrieving track data before downloading
    tracks.
- Download playlists.
  - Since Scarch uses youtube-dl under the hood, it can handle links
    to playlists just fine.
- Download all user uploads or user likes.
  - User uploads and user likes are just playlists! You can download
    them with the urls `https://soundcloud.com/user/tracks` or
    `https://soundcloud.com/user/likes`
- Organize downloaded files.
  - Scarch organizes downloaded tracks into folders by uploader.
- Download track metadata and thumbnails.
  - Each track is stored in its own folder, along with the track
    metadata, and track thumbnail if one exists.
  - Scarch saves metadata in a json file for easier processing
    later.
  - Unfortunately, Scarch can't yet store the metadata directly in
    the audio file itself. I may add this feature later on, and if
    I do, I'll also add a command to merge the `metadata.json`
    files with the audio files, rather than requiring a
    re-download.

## Build / Install

Scarch requires the `youtube-dl` command be available to function. If
you're using Linux, there is likely a package for it in your package
manager. Mac users can also install it with homebrew: `brew install
youtube-dl`. If you're a Windows user, download the `.exe` file  from
[https://rg3.github.io/youtube-dl/download.html](https://rg3.github.io/youtube-dl/download.html)
and place it in the Scarch project folder.

Scarch is written in Haskell, and can be built easily using the `stack` build tool.

If you don't have `stack` installed, see [Stack - Install and
Upgrade](https://docs.haskellstack.org/en/stable/install_and_upgrade/) for
instructions on how to install stack.

Once stack is installed, navigate to the Scarch project directory in a terminal
and run `stack setup`. This will download the Haskell compiler if it's not
already installed.

Run `stack build` to build Scarch.

Run `stack install` to install the `scarch` command line tool. Stack will tell
you where it's been installed.

Alternatively, you can use `stack exec scarch -- <args/options>` to execute
Scarch without `stack` copying it outside of the project directory.


## Usage

Run `scarch -h` for usage information. The output of `scarch -h` has been
copied here for your convenience.

```
Scarch - A SoundCloud archiver

Usage: scarch [-j|--jobs NUM_JOBS] ((-f|--file FILE) | URLS...)
  Scarch is a wrapper around youtube-dl which can archive content from
  SoundCloud faster and more conveniently than using youtube-dl by itself.
  Scarch will download tracks and playlists from URLs supplied on the command
  line, and from URLs listed in files specified with the -f flag. Downloaded
  tracks are orgnanized into folders by uploader. Each track is stored in its
  own folder, along with the track metadata, and track thumbnail if one exists.

Available options:
  -j,--jobs NUM_JOBS       Specify the number of files to download
                           simultaneously. Raising this number may increase
                           speed, but it will also take more processing power
                           and more bandwidth. (default: 8)
  -f,--file FILE           Input file containing one URL per line. Multiple
                           files may be specified by using -f more than once. A
                           filename of '-' tells Scarch to read from STDIN.
  URLS...                  URLs to download. If a URL points to a playlist, all
                           tracks in the playlist will be downloaded. A
                           SoundCloud user's tracks and likes are playlists too!
                           Use https://soundcloud.com/username/tracks or
                           https://soundcloud.com/username/likes to download
                           them.
  -h,--help                Show this help text
```
