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

  # nix.package = pkgs.nix;
  nixpkgs = {
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.packages = with pkgs; [

    # fonts
    noto-fonts
    vistafonts
    go-font
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    hack-font
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" "Hack" ]; })
    dina-font
    source-code-pro
    lohit-fonts.devanagari
    cascadia-code
    # END fonts

    aspell
    atuin
    autoconf
    automake
    bison
    chromium
    cmake
    cmake
    corepack
    delta
    devenv
    docker
    doppler
    editorconfig-core-c
    extra-cmake-modules
    eza
    file
    fmt
    fortune
    gcc
    gdb
    gdbgui
    gettext
    gh
    gnumake
    go
    gperf
    htop
    inkscape
    jq
    json_c
    k9s
    kitty
    kubectl
    libcap
    libtool
    m4
    mysql84
    nasm
    nix-index
    nodejs
    pavucontrol
    pkg-config
    postgresql_16
    postman
    insomnia
    pyenv
    redis
    ripgrep
    rofi
    slack
    steam-run
    thefuck
    tmux
    tmuxinator
    tmuxinator
    tree
    unzip
    hydroxide
    xclip
    yarn
    yq
    xdiskusage
    gitoxide
    bacon
    live-server
    sxhkd
    dmenu
    xtitle
    lemonbar
    xdo
    xcape
    dig
    nyxt
    qemu_kvm
    (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
  ];


  systemd.user.services.hydroxide = {
    Unit.Description = "Hydroxide - ProtonMail Bridge";
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${pkgs.hydroxide}/bin/hydroxide serve";
      Restart = "always"; 
      RestartSec = 5;
    };
  };
  
  home.file = {
    ".config/tmuxinator/backend.yaml".source = ../tmuxinator/backend.yaml;
    ".config/awesome/rc.lua".source = ../awesome/rc.lua;    
    ".config/emacs/init.el".source = ../emacs.d/init.el;
    ".config/emacs/prelude.el".source = ../emacs.d/prelude.el;
    ".config/emacs/early-init.el".source = ../emacs.d/early-init.el;
    ".config/emacs/navapali-maps.el".source = ../emacs.d/navapali-maps.el;
    ".config/emacs/copilot.el".source = ../emacs.d/copilot.el;
    ".config/emacs/packages/sxhkd-mode.el".source = ../emacs.d/packages/sxhkd-mode.el;
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
    ".tmux" = {
      source: ../tmux/tmux;
      recursive = true;
    };
    ".Xmodmap".source = ../Xmodmap;
  };

  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
  home.shellAliases = {
    ls="exa";
  };
  programs.zoxide.enable = true;
  programs.bash = {
    enable = true;
    bashrcExtra = ''
export HISTCONTROL=ignoreboth:erasedups

# bash config for wasmenv
# copy this to ~/.bashrc
WASMENV_DIR="/home/ayys/.config/wasmenv"
[ -s "/home/ayys/.config/wasmenv/wasmenv.sh" ] && source "/home/ayys/.config/wasmenv/wasmenv.sh"
export WASMENV_DIR PANEL_FIFO PANEL_HEIGHT PANEL_FONT PANEL_WM_NAME
'';
  };


  home.file.".local/bin/rb" = {
    text = ''
    #!/bin/sh
    nixos-rebuild --flake $HOME/git/dots/nix#ayys --use-remote-sudo switch
  '';
    executable = true;
  }; 

  home.file.".local/bin/gc" = {
    text = ''  
    #!/bin/sh
    nix-collect-garbage -d
  '';
    executable = true;
  }; 


  home.file.".local/bin/linear-firefox" = {
    text = ''
    #!/bin/sh
    firefox --new-window --kiosk "https://linear.app"
  '';
    executable = true;
  }; 

  home.file.".local/bin/re" = {
    text = ''
    #!/bin/sh
    systemctl restart --user emacs
  '';
    executable = true;
  }; 

  home.file.".local/bin/tm-be" = {
    text = ''
    #!/bin/sh
    tmuxinator backend
  '';
    executable = true;
  };

  home.sessionPath = [
    "$HOME/.local/bin"    
  ];

  programs.direnv.enable = true;

  programs.git = {
    enable = true;
    package = pkgs.git;
    userName  = "ayys";
    extraConfig = {
      push = { autoSetupRemote = true; };
    };
  };

  systemd.user.services.set-wallpaper = {
    Unit.Description = "Set wallpaper using feh";
    Install.WantedBy = [ "graphical-session.target" ];
    Service.ExecStart = "${pkgs.writeShellScript "wallpaper-switch" ''
        #!bash
        ${pkgs.feh}/bin/feh --bg-scale --randomize ~/git/dots/wallpapers/
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
