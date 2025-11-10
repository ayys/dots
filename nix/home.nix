{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.

  home = {
    # The username for the user to create.
    username = "ayys";

    # The home directory for the user to create.
    homeDirectory = "/home/ayys";
  };


  home.stateVersion = "25.05"; # Please read the comment before changing.

  home.packages = with pkgs; [
    (writeShellScriptBin "switch_kbdlayout" ''
# switch between us and np keyboard layouts

if [ $(setxkbmap -query | grep layout | cut -d ' ' -f 6) = "us" ]; then
    setxkbmap np
else
    setxkbmap us
fi
    '')
  ];


  systemd.user.services.hydroxide = {
    Unit.Description = "Hydroxide - ProtonMail Bridge";
    Install.WantedBy = [ "default.target" ];
    Service = {
      Environment = "HYDROXIDE_CARDDAV_PORT=8082";
      ExecStart = "${pkgs.hydroxide}/bin/hydroxide -carddav-port 8081 serve";
      Restart = "always"; 
      RestartSec = 5;
    };
  };
  
  home.file = {
    ".config/tmuxinator/backend.yaml".source = ../tmuxinator/backend.yaml;
    ".config/awesome/rc.lua".source = ../awesome/rc.lua;
    ".config/emacs" = {
      source = ../emacs.d;
      recursive = true;
    };
    ".config/bspwm/bspwmrc".source = ../bspwm/bspwmrc;
    ".config/sxhkd/sxhkdrc".source = ../bspwm/sxhkdrc;
    ".config/hypr/hyprland.conf".source = ../hypr/hyprland.conf;
    ".config/waybar/config.jsonc".source = ../waybar/config.jsonc;
    ".config/waybar/style.css".source = ../waybar/style.css;
    ".config/kitty/kitty.conf".source = ../kitty/kitty.conf;
    ".config/tz.txt".source = ../bspwm/tz.txt;
    ".local/bin/panel".source = ../bspwm/panel;
    ".local/bin/panel_bar".source = ../bspwm/panel_bar;
    ".local/bin/panel_colors".source = ../bspwm/panel_colors;
    ".tmux.conf".source = ../tmux/tmux.conf;
    ".config/nyxt" = {
      source = ../nyxt;
      recursive = true;
    };
    ".Xmodmap".source = ../Xmodmap;
    ".local/bin/rb" = {
      text = ''
    #!/bin/sh
    nixos-rebuild --flake $HOME/git/dots/nix#ayys --sudo switch
  '';
      executable = true;
    };
    ".local/bin/gc" = {
      text = ''  
    #!/bin/sh
    nix-collect-garbage -d
  '';
      executable = true;
    };
    ".local/bin/re" = {
      text = ''
    #!/bin/sh
    systemctl restart --user emacs
  '';
      executable = true;
    };

    ".emacs" = {
      text = ''
(setq user-emacs-directory (file-truename "~/.config/emacs"))
  '';
    };
    ".local/bin/tm-be" = {
      text = ''
    #!/bin/sh
    tmuxinator backend
  '';
      executable = true;
    };
  };

  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
  home.shellAliases = {
    ls="eza";
  };


  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };

  programs.git = {
    enable = true;
    package = pkgs.git;
    lfs.enable = true;

    settings = {
      user = {
        name = "Ayush Jha";
        email = "ayys@duck.com";
      };

      pull.rebase = true;

      column.ui = "auto";

      branch.sort = "-committerdate";
      tag.sort = "version:refname";

      help.autocorrect = "prompt";

      commit.verbose = true;

      rerere = {
        enabled = true;
        autoupdate = true;
      };

      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
      };

      push = {
        default = "simple";
        autoSetupRemote = true;
        followTags = true;
      };

      diff = {
        algorithm = "histogram";
        colorMoved = "zebra";
        mnemonicPrefix = true;
        renames = true;
      };
    };

    # 1. default (fallback) personal config

    signing.signByDefault = true;
    signing.key = "8D0723A80F4F6443";

    includes = [
      {
        condition = "gitdir:~/git/lyric/";
        contents = {
          user = {
            email = "ayush.jha@lyric.ai";
            name = "Ayush Jha";

            # lyric gpg key
            signingKey = "40C47431C735861E";
          };
          commit.gpgSign = true;
        };
      }
      {
        condition = "gitdir:~/git/mvv/";
        contents = {
          user = {
            email = "ayush@multiversal.ventures";
            name = "Ayush Jha";
            signingKey = "A938C8A25F6F962F";
          };
          commit = {
            gpgSign = true;
          };
        };
      }
    ];

  };

  services.gpg-agent.enable = true;

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };


  programs.bash = {
    enable = true;
    bashrcExtra = ''
export HISTCONTROL=ignoreboth:erasedups

# bash config for wasmenv
# copy this to ~/.bashrc
WASMENV_DIR="/home/ayys/.config/wasmenv"
[ -s "/home/ayys/.config/wasmenv/wasmenv.sh" ] && source "/home/ayys/.config/wasmenv/wasmenv.sh"
export WASMENV_DIR PANEL_FIFO PANEL_HEIGHT PANEL_FONT PANEL_WM_NAME

# GUIX_PROFILE="/home/ayys/.guix-profile"
# . "$GUIX_PROFILE/etc/profile"

# Load pyenv automatically by appending
# the following to 
# ~/.bash_profile if it exists, otherwise ~/.profile (for login shells)
# and ~/.bashrc (for interactive shells) :

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - bash)"

# Restart your shell for the changes to take effect.
'';
  };


  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  programs.direnv.enable = true;

  
  systemd.user.services.set-wallpaper = {
    Unit.Description = "Set wallpaper using feh";
    Install.WantedBy = [ "graphical-session.target" ];
    Service.ExecStart = "${pkgs.writeShellScript "wallpaper-switch" ''
        #!bash
        # ${pkgs.feh}/bin/feh --no-fehbg --bg-scale --randomize ~/git/dots/wallpapers/
        ${pkgs.feh}/bin/feh --no-fehbg --bg-scale ~/git/dots/wallpapers/034.png
      ''}";
  };

  systemd.user.services.xcape-control-config = {
    Unit.Description = "config xcape to treat tapping control as capslock";
    Install.WantedBy = [ "graphical-session.target" ];
    Service.ExecStart = "${pkgs.writeShellScript "xcape-control-escape" ''
        #!bash
        ${pkgs.xcape}/xcape -e 'Control_L=Escape;Shift_L=Shift_L|Shift_L' -f -t 400
      ''}";
  };

  systemd.user.timers.set-wallpaper = {
    Unit.Description = "Auto wallpaper changer";
    Timer = {
      OnBootSec = "10s";
      OnUnitInactiveSec = "10s";
      Unit = "set-wallpaper.service";
    };
    Install.WantedBy = ["timers.target" "graphical-session.target"];
  };

  fonts.fontconfig.enable = true;

  programs.firefox = {
    enable = true;
    languagePacks = [
      "ne-NP" "en-US"
    ];
    profiles.default = {
      settings = {
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "browser.startup.homepage" = "https://illegal-in-nepal.org";
        "sidebar.verticalTabs" = true;
        "browser.aboutConfig.showWarning" = false;

        "browser.download.useDownloadDir" = false;
        "browser.download.dir" = "/home/ayys/dwn";
        "browser.download.folderList" = 2; # this is needed to tell firefox to switch to the dir above
        "browser.ctrlTab.sortByRecentlyUsed" = true;

        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
        "browser.newtabpage.activity-stream.impressionId" = "{eb042959-5732-43b2-9673-b40a0e776344}";
        "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
        "browser.newtabpage.activity-stream.showSearch" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.pinned" = ''[{"url":"https://search.nixos.org/packages","label":"NixOS Search - Packages - brow.sh"},{"url":"https://github.com/","label":"GitHub"},{"url":"https://claimsxten.atlassian.net/jira/your-work","label":"Jira"},{"url":"https://gemini.google.com/"},{"url":"https://www.reddit.com/"},{"url":"https://mail.proton.me/"},{"url":"https://savvytime.com/"},{"url":"https://miti.bikram.io/"}]'';

        
      };
      userChrome = builtins.readFile ../firefox/userChrome.css;
    };
  };


  services.picom = {
    enable = true;
    backend = "glx"; # or "xrender" if glx gives you issues
    vSync = true;

    # Optional: additional tweaks
    settings = {

      # Subtle fading animations
      fading = true;
      fade-delta = 10;         # Lower = slower fade (10ms between steps)
      fade-in-step = 0.05;     # 0.0–1.0, controls fade-in speed
      fade-out-step = 0.05;    # same for fade-out
      no-fading-openclose = false;


      # Rounded corners for a modern look
      corner-radius = 6;

      # Optional: Shadow tweaks (less harsh)
      shadow = true;
      shadow-radius = 10;
      shadow-opacity = 0.2;
      shadow-offset-x = -5;
      shadow-offset-y = -5;

      # Avoid flickering
      unredir-if-possible = false;      

      refresh-rate = 75;  # match your monitor
    };
  };

  
  systemd.user.targets.graphical-session.target.enable = true; # Ensure the graphical session target is enabled  


  # Let Home Manager install and manage itself.
  programs.home-manager = {
    enable = true;
  };
}
