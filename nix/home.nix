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

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      ll = "ls -l";
    };
    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "thefuck" ];
      theme = "robbyrussell";
    };
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
    dolphin
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
    git
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
    poetry
    poetry
    postgresql_16
    postman
    pyenv
    qemu_full
    redis
    ripgrep
    rofi
    rustup
    slack
    steam-run
    thefuck
    tmux
    tmuxinator
    tmuxinator
    tree
    unzip
    xclip
    yarn
    yq
    xdiskusage
(google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
  ];


  home.file = {
    ".config/awesome/rc.lua".source = ../awesome/rc.lua;
    ".emacs.d/init.el".source = ../emacs.d/init.el;
    ".emacs.d/early-init.el".source = ../emacs.d/early-init.el;
    ".emacs.d/navapali-maps.el".source = ../emacs.d/navapali-maps.el;
    ".emacs.d/copilot.el".source = ../emacs.d/copilot.el;
    ".emacs.d/snippets".source = ../emacs.d/snippets;
    ".config/bspwm/bspwmrc".source = ../bspwm/bspwmrc;
    ".config/sxhkd/sxhkdrc".source = ../bspwm/sxhkdrc;
    ".config/hypr/hyprland.conf".source = ../hypr/hyprland.conf;
    ".config/waybar/config.jsonc".source = ../waybar/config.jsonc;
    ".config/waybar/style.css".source = ../waybar/style.css;
    ".config/kitty/kitty.conf".source = ../kitty/kitty.conf;
    ".Xmodmap".source = ../Xmodmap;
  };

  home.sessionVariables = {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
  home.shellAliases = {
    rebuild = "nixos-rebuild --flake $HOME/git/dots/nix#ayys --use-remote-sudo switch";
    rb = "rebuild";
    gc = "nix-collect-garbage";
    git-rm-ws = "git diff -U0 -w --no-color | git apply --cached --ignore-whitespace --unidiff-zero -";
  };
  programs.zoxide.enable = true;
  programs.bash = {
    enable = true;
  };

  programs.firefox = {
    enable  = true;
    package = pkgs.firefox-bin;
  };

  home.file.".local/bin/linear-firefox".text = ''
    #!/bin/sh
    firefox --new-window --kiosk "https://linear.app"
  '';

  programs.direnv.enable = true;

  programs.git = {
    enable = true;
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
        ${pkgs.feh}/bin/feh --bg-scale --randomize ~/git/dotfiles/Wallpapers/
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

  systemd.user.targets.graphical-session.target.enable = true; # Ensure the graphical session target is enabled  


  # Let Home Manager install and manage itself.
   programs.home-manager = {
     enable = true;
   };
}
