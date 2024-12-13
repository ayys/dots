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

  # Install unfree programs
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
    (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
    aspell
    atuin
    autoconf
    automake
    bison
    cmake
    corepack
    delta
    docker
    dolphin
    doppler
    editorconfig-core-c
    eza
    file
    fortune
    gcc
    gettext
    gh
    git
    gnumake
    go
    gperf
    htop
    k9s
    kitty
    kubectl
    libcap
    libtool
    m4
    nodejs
    poetry
    postgresql_16
    pyenv
    redis
    ripgrep
    rofi
    rustup
    slack
    steam-run
    tmux
    tmuxinator
    tmuxinator
    tree
    unzip
    yarn
    poetry
    jq
    yq
    mysql84
    postman
    inkscape
    cmake
    extra-cmake-modules
    pkg-config
    fmt
    json_c
    devenv
    xclip
    nasm
    qemu_full
    pavucontrol
    chromium
    nix-index
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
    rebuild = "nixos-rebuild --flake $HOME/git/dots/nix#ayys --use-remote-sudo switch ; nix-collect-garbage";
    rb = "rebuild";
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
  programs.home-manager.enable = true;
}
