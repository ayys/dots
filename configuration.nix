{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ayys"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Kathmandu";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Enable the X11 windowing system.
  # You can disable this if you're only using the Wayland session.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.defaultSession = "plasmax11";
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.wayland.enable = false;
  

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      layout = "us";
      variant = "";
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ayys = {
    isNormalUser = true;
    description = "Ayush";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      mplus-outline-fonts.githubRelease
      dina-font
      source-code-pro
      lohit-fonts.devanagari
      proggyfonts
    ];
  };

  home-manager.backupFileExtension = "backup";
  home-manager.useGlobalPkgs = true;
  home-manager.users.ayys = {pkgs, ...}: {
    home.packages = with pkgs; [
      htop
      git
      fortune
      editorconfig-core-c
      ripgrep
      nodejs
      yarn
      slack
      tmux
      tmuxinator
      pyenv
      cmake
      aspell
      kubectl
      google-cloud-sdk
      k9s
      postgresql_16
      tmuxinator
      atuin
      eza
      gnumake
      gcc
      libtool
      delta
      gh
      killall
    ];

    programs.zoxide.enable = true;

    home.file = {
      ".config/awesome/rc.lua".source = /home/ayys/git/dots/awesome/rc.lua;
      ".emacs.d/init.el".source = /home/ayys/git/dots/emacs.d/init.el;
      ".emacs.d/early-init.el".source = /home/ayys/git/dots/emacs.d/early-init.el;
      ".emacs.d/navapali-maps.el".source = /home/ayys/git/dots/emacs.d/navapali-maps.el;
      ".emacs.d/packages".source = /home/ayys/git/dots/emacs.d/packages;
      ".emacs.d/copilot.el".source = /home/ayys/git/dots/emacs.d/copilot.el;
      ".emacs.d/snippets".source = /home/ayys/git/dots/emacs.d/snippets;
      ".config/bspwm/bspwmrc".source = /home/ayys/git/dots/bspwm/bspwmrc;
      ".config/sxhkd/sxhkdrc".source = /home/ayys/git/dots/bspwm/sxhkdrc;
      ".config/hypr/hyprland.conf".source = /home/ayys/git/dots/hypr/hyprland.conf;
      ".config/waybar/config.jsonc".source = /home/ayys/git/dots/waybar/config.jsonc;
      ".config/waybar/style.css".source = /home/ayys/git/dots/waybar/style.css;
      ".Xmodmap".source = /home/ayys/git/dots/Xmodmap;
    };
    programs.bash = {
      enable = true;
    };
    home.shellAliases = {
      rebuild = "nixos-rebuild --flake $HOME/git/dots#ayys -I nixos-config=$HOME/git/dots/configuration.nix --use-remote-sudo switch";
    };
    # The state version is required and should stay at the version you
    # originally installed.
    home.stateVersion = "24.05";    
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "ayys";

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget

    (pkgs.emacsWithPackagesFromUsePackage {
      config = /home/ayys/.emacs.d/init.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      package = pkgs.emacs-unstable;
    })
  ];

  services.emacs.package = pkgs.emacs-unstable;
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
