{ config, pkgs, inputs,  ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];


  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  nix.settings.trusted-users = ["root" "@wheel"];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  ## Networking logic
  networking.hostName = "ayys"; # Define your hostname.
  networking.networkmanager.enable = true;
  

  # Set your time zone.
  time.timeZone = "Asia/Kathmandu";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.inputMethod = {
    enable = true;
    type = "ibus";    # Set IBus as the input method framework
    ibus.engines = with pkgs.ibus-engines; [
      m17n   # Enable m17n engine (which supports typing in Devanagari)
    ];
  };

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.defaultSession = "plasmax11";
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.wayland.enable = false;
  services.xserver.displayManager.sessionCommands = ''
  export GTK_IM_MODULE=ibus
  export QT_IM_MODULE=ibus
  export XMODIFIERS=@im=ibus
  ibus-daemon -drx
'';
  
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../emacs.d/init.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      package = pkgs.emacs-git;
      extraEmacsPackages = epkgs: [
      ];
    };
  };

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us";
      variant = "";
    };
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks # is the package manager for Lua modules
        luadbi-mysql # Database abstraction layer
      ];
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
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      hack-font
      (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" "Hack" ]; })
      python311Packages.python
      python311Packages.ipython
      python311Packages.pip
      python311Packages.python-lsp-server
      python311Packages.requests
      python311Packages.pylsp-mypy
      python311Packages.python-lsp-ruff
      hackgen-nf-font
      mplus-outline-fonts.githubRelease
      dina-font
      source-code-pro
      lohit-fonts.devanagari
      proggyfonts
      inputs.ayys-st.packages."${pkgs.system}".st
      inputs.wasmenv.packages."${pkgs.system}".wasmenv
      xorg.xmodmap
      feh
      meson
      wasmer
      man-pages
      man-pages-posix
    ];
  };


  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = false;
  services.displayManager.autoLogin.user = "ayys";

  # programs.hyprland = {
  #   enable = true; 
  #   xwayland.enable = true;
  # };
  # programs.waybar.enable = true;

  # # Hint Electon apps to use wayland
  # environment.sessionVariables = {
  #   NIXOS_OZONE_WL = "1";
  # };
  services.dbus.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    killall
  ];

  documentation.dev.enable = true;

  documentation.man = {
    man-db.enable = false;
    mandoc.enable = true;
  };
  
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/6fd1f939e4453206d131744aee904c019f216ecd.tar.gz";
      sha256 = "060gc3vv5552iqv6ljclnv2f784l77xqd2ilccfb5790xkd5i08m";
    }))
    (self: super: {
      waybar = super.waybar.overrideAttrs (oldAttrs: {
        mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
      });
    })
  ];

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged programs
    # here, NOT in environment.systemPackages
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  systemd.user.services.xrandr = {
    description = "Set xrandr settings on startup";
    serviceConfig = {
      ExecStart = "${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-1 --rotate normal";
    };
    wantedBy = [ "graphical-session.target" ];
  };

  nix.settings.auto-optimise-store = true;
  nix.optimise.automatic = true;


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  virtualisation.docker.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8000 3000 22 ];
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
