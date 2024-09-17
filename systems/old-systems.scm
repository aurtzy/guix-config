
;; NOTE: this is an old system configuration. As equivalent configurations are
;; ported to newer setups for active use again, corresponding sections here
;; will be carelessly deleted to get an idea of what's left to port. this code
;; will likely not even work.

(define* (build-os
    #:key hostname tmz uname os-dev efi-dev
          nvidia laptop vm plasma gnome
  ) 
  (fold include-configuration
    (operating-system

      (packages (cons*
        ;; disk
        gptfdisk
        ;; cryptsetup
        cryptsetup
        ;; xorg
        xset

        %base-packages
      ))
    )
    
    (list
      
      (cons vm (lambda (os)
        (operating-system
          (inherit os)
          (packages (cons*
            ;; virtualization
            qemu
            virt-manager
            ;; firmware
            ovmf
            ;;
            (operating-system-packages os)
          ))
          (kernel-arguments (cons*
            "intel_iommu=on"
            "iommu=pt"
            "rd.driver.pre=vfio-pci"
            "vfio-pci.ids=10de:2182,10de:1aeb,10de:1aec,10de:1aed"
            %default-kernel-arguments ;; couldn't figure out how to inherit kernel-arguments
          ))
          (services (cons*
            ;; virtualization
            (service libvirt-service-type
              (libvirt-configuration
                (unix-sock-group "libvirt")
              )
            )
            (service virtlog-service-type)
            ;; Fix virt-manager not finding UEFI firmware
            ;; Credits for this:
            ;; u/aerique's reddit comment from: r/GUIX/comments/s18slc/virtmanager_and_ovmf
            ;; This debian issue at: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=764918
            (extra-special-file "/usr/share/OVMF/OVMF_CODE.fd"
              (file-append ovmf "/share/firmware/ovmf_x64.bin")
            )
            (extra-special-file "/usr/share/OVMF/OVMF_VARS.fd"
              (file-append ovmf "/share/firmware/ovmf_x64.bin")
            )
            (extra-special-file "/etc/libvirt/hooks/kvm.conf"
              (local-file "./local/vm/kvm.conf")
            )
            (extra-special-file "/etc/libvirt/hooks/start.sh"
              (local-file "./local/vm/start.sh")
            )
            (extra-special-file "/etc/libvirt/hooks/revert.sh"
              (local-file "./local/vm/revert.sh")
            )
            ;;
            (operating-system-user-services os)
          ))
        )
      ))
    )
  )
)
