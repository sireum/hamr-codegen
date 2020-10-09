# Simple Producer-Filter-Consumer

![arch](diagrams/arch.png)

## Required AADL Model Property Annotations

- `HAMR::Bit_Codec_Raw_Connections => true` must be added to the top level system (e.g. [PFC_Sys.Impl](PFC.aadl#L39))

- The encoded size for each data type used in connections between threads must be 
  specified using the `HAMR::Bit_Codec_Max_Size` property.  For example, 
  [Mission](PFC.aadl#L15-L23)
  is used in connections [c1](PFC.aadl#L51) and [c2](PFC.aadl#L52) so its defintion
  must specify the [Bit_Codec_Max_Size](PFC.aadl#L22).  
  
  This property
  only needs to be attached to the top level data component (e.g. array subtypes
  and record field types do not need to be modified if they are not directly
  used in a connection)
  
  Wrapper data components would currently (2020.05.09) need to be introduced to handle connections using 
  AADL 
  [Base_Types](https://github.com/osate/osate2/blob/master/core/org.osate.contribution.sei/resources/packages/Base_Types.aadl)
  so that the property can be attached.
  
## Behavior code provided via C
  
  Behavior code and supporting APIs are located in the following directories
  
  - [slang_embedded_excludes/src/c/ext-c/Producer_Impl](slang_embedded_excludes/src/c/ext-c/Producer_Impl)

  - [slang_embedded_excludes/src/c/ext-c/Filter_Impl](slang_embedded_excludes/src/c/ext-c/Filter_Impl)

  - [slang_embedded_excludes/src/c/ext-c/Consumer_Impl](slang_embedded_excludes/src/c/ext-c/Consumer_Impl)
  
### Running example under Linux
  
  ```
  sporadic/slang_embedded_excludes/bin/transpile.sh
  sporadic/slang_embedded_excludes/bin/compile-linux.sh 
  sporadic/slang_embedded_excludes/bin/run-linux.sh 
  sporadic/slang_embedded_excludes/bin/stop.sh 
  ```
  
  Output should be similar to what is seen when [running under QEMU](#running-example-under-sel4qemu)
  
### Running example under seL4/QEMU
  
  ```
  sporadic/slang_embedded_excludes/bin/transpile-sel4.sh
  rm -rf ~/CASE/camkes/projects/camkes/apps/CAmkES_seL4
  sporadic/slang_embedded_excludes/src/c/CAmkES_seL4/bin/run-camkes.sh
  ```
  
  Output should resemble
  
  ```
...
PFC_Sys_Impl_Instance_proc_consumer: Received [FF000000]
PFC_Sys_Impl_Instance_proc_filter:   Rejected [00FF0000]
PFC_Sys_Impl_Instance_proc_filter:   Rejected [0000FF00]
PFC_Sys_Impl_Instance_proc_consumer: Received [000000FF]
PFC_Sys_Impl_Instance_proc_consumer: Received [FF000000]
PFC_Sys_Impl_Instance_proc_filter:   Rejected [00FF0000]
PFC_Sys_Impl_Instance_proc_filter:   Rejected [0000FF00]
```

## Behavior code provided via Slang + BitCodec

### Regenerating BitCodec Resources

The bit codec spec is located at [data/Mission.sc](data/Mission.sc)

To regenerate the bit codec resources run the following script

```sporadic/slang_embedded_bitcodec/bin/bcgen.sh```


This will produce artifacts in the following directories

- [data](data) 

- [slang_embedded_bitcodec/src/main/data/pfc_project/PFC](slang_embedded_bitcodec/src/main/data/pfc_project/PFC)

### Slang behavior code using bit codec

Refer to the following Slang classes

- [Producer_Impl](slang_embedded_bitcodec/src/main/component/pfc_project/PFC/Producer_Impl.scala)

- [Filter_Impl](slang_embedded_bitcodec/src/main/component/pfc_project/PFC/Filter_Impl.scala)

- [Consumer_Impl](slang_embedded_bitcodec/src/main/component/pfc_project/PFC/Consumer_Impl.scala)

### Running example under JVM

- Open the [slang_embedded_bitcodec](slang_embedded_bitcodec) directory in Sireum IVE

- In Sireum IVE, right click [Demo](slang_embedded_bitcodec/src/main/architecture/pfc_project/Demo.scala) 
  in the Project View and choose ``Run 'Demo'``

Output should be similar to what is seen when [running under QEMU](#running-example-under-sel4qemu-1)

### Running example under Linux

```
sporadic/slang_embedded_bitcodec/bin/transpile.sh 
sporadic/slang_embedded_bitcodec/bin/compile-linux.sh 
sporadic/slang_embedded_bitcodec/bin/run-linux.sh 
sporadic/slang_embedded_bitcodec/bin/stop.sh 
```

Output should be similar to what is seen when [running under QEMU](#running-example-under-sel4qemu-1)

### Running example under seL4/QEMU

```
sporadic/slang_embedded_bitcodec/bin/transpile-sel4.sh 
rm -rf ~/CASE/camkes/projects/camkes/apps/CAmkES_seL4
sporadic/slang_embedded_bitcodec/src/c/CAmkES_seL4/bin/run-camkes.sh
```

Output should resemble

```
...
PFC_Sys_Impl_Instance_proc_producer: Sending [000000000000000000000000800000008000000080000000400000004000000040000000]
PFC_Sys_Impl_Instance_proc_filter: Filtered out [000000000000000000000000800000008000000080000000400000004000000040000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [C0000000C0000000C0000000200000002000000020000000A0000000A0000000A0000000]
PFC_Sys_Impl_Instance_proc_consumer: received [C0000000C0000000C0000000200000002000000020000000A0000000A0000000A0000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [600000006000000060000000E0000000E0000000E0000000100000001000000010000000]
PFC_Sys_Impl_Instance_proc_consumer: received [600000006000000060000000E0000000E0000000E0000000100000001000000010000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [FFFFFFFF0000000000000000000000010000000100000001000000020000000200000002]
PFC_Sys_Impl_Instance_proc_filter: Filtered out [FFFFFFFF0000000000000000000000010000000100000001000000020000000200000002]
PFC_Sys_Impl_Instance_proc_producer: Sending [000000000000000000000000800000008000000080000000400000004000000040000000]
PFC_Sys_Impl_Instance_proc_filter: Filtered out [000000000000000000000000800000008000000080000000400000004000000040000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [C0000000C0000000C0000000200000002000000020000000A0000000A0000000A0000000]
PFC_Sys_Impl_Instance_proc_consumer: received [C0000000C0000000C0000000200000002000000020000000A0000000A0000000A0000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [600000006000000060000000E0000000E0000000E0000000100000001000000010000000]
PFC_Sys_Impl_Instance_proc_consumer: received [600000006000000060000000E0000000E0000000E0000000100000001000000010000000]
PFC_Sys_Impl_Instance_proc_producer: Sending [FFFFFFFF0000000000000000000000010000000100000001000000020000000200000002]
PFC_Sys_Impl_Instance_proc_filter: Filtered out [FFFFFFFF0000000000000000000000010000000100000001000000020000000200000002]

```
