package org.sireum.hamr.arsit;

import org.sireum.None$;
import org.sireum.Some$;

public class ArsitBridge {

    // should match Cli.Ipcmech
    public enum IPCMechanism {
        MessageQueue,
        SharedMemory
    }

    // should match Cli.Platform
    public enum Platform {
        JVM,
        Linux,
        Cygwin,
        MacOS,
        SeL4
    }

    public static <T> org.sireum.Option<T> sireumOption(T o) {
        if(o != null) {
            return Some$.MODULE$.apply(o);
        } else {
            return None$.MODULE$.apply();
        }
    }
}