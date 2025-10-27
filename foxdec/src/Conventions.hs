{-|
Module      : Conventions 
Description : In this file, we eumerate assumptions made on calling conventions and over external functions.
-}

module Conventions where

import Base
import qualified Data.Map as M
import Debug.Trace

import Data.X86.Register


-- | A list of function names of functions that never return.
is_exiting_function_call f =
  let f' = strip_GLIBC f in 
    f' `elem` [
      "exit", "_exit", "__exit", "___exit",
      "_error","__error", 
      "__stack_chk_fail", "___stack_chk_fail",
      -- "__overflow", 
      "abort", "_abort",
      "_fail",
      "halt",
      "_assert_fail", "__assert_fail", "___assert_fail", "___assert_rtn",
      "err", "verr", "errc", "verrc", "errx", "verrx", 
      "_err", "_verr", "_errc", "_verrc", "_errx", "_verrx",
      "obstack_alloc_failed_handler", "isc_assertion_failed", "isc_error_fatal",
      "PyExc_SystemExit", "pthread_exit", "error_exit",
      "__longjmp_chk", "siglongjmp"
    ]

strip_GLIBC = takeUntilString "@GLIBC" . takeUntilString "@@GLIBC"


-- TODO syscalls differ per OS
is_terminal_syscall i = i `elem`
  [ --   1 -- exit (FREEBSD)
    -- , 431 -- thr_exit (FREEBSD)
    15 -- rt_sigreturn
  , 60 -- exit
  , 231 -- exit_group
  ]



-- | Overview of sections with instructions.
sections_with_instructions = [
   ("__TEXT","__text"), -- TODO ELF
   ("__TEXT","__stubs"),
   ("__TEXT","__stub_helper"),
   ("__DATA_CONST","__got"),
   ("__DATA","__la_symbol_ptr"),
   ("__DATA","__nl_symbol_ptr"),

   ("",".text"),
   ("",".init"),
   ("",".plt"),
   ("",".iplt"),
   ("",".plt.got"),
   ("",".plt.sec"),
   ("",".fini")
 ]

-- | Sections in the following list are assumed not to be modifiable during execution, i.e., constant.
section_is_unwritable s@(segname,sect_name) = 
  or [
    segname `elem` ["__TEXT","__DATA_CONST"],
    s `elem` [ ("__DATA","__got"), ("__DATA","__const"), ("",".rodata") ],
    s `elem` sections_with_instructions
  ]



-- | A list if registers that are non-volatile, i.e., that must be preserved by a function (callee-saved)
callee_saved_registers = map Reg64 [RBX, RBP, RSP, R12, R13, R14, R15]


-- | A list of registers that may be used for return values
return_registers = map Reg64 [RAX]






-- Registers possibly overwritten after a syscall
regs_clobbered_by_syscall = map Reg64 [RAX, R11, RCX]

-- Register input parameters of syscalls in order
all_input_regs_of_syscalls = map Reg64 [RDI,RSI,RDX,R10,R8,R9]

-- Register input parameters in order
all_input_regs_of_functions = map Reg64 [RDI,RSI,RDX,RCX,R8,R9]

-- Per syscall, its name and it argcount
num_of_input_registers_of_sys_call :: Int -> (String,Int)
num_of_input_registers_of_sys_call = sc
 where
  sc 0      = ("read"                       ,3) 
  sc 1      = ("write"                      ,3) 
  sc 2      = ("open"                       ,3) 
  sc 3      = ("close"                      ,1) 
  sc 4      = ("stat"                       ,2) 
  sc 5      = ("fstat"                      ,2) 
  sc 6      = ("lstat"                      ,2) 
  sc 7      = ("poll"                       ,3) 
  sc 8      = ("lseek"                      ,3) 
  sc 9      = ("mmap"                       ,6) 
  sc 10     = ("mprotect"                   ,3) 
  sc 11     = ("munmap"                     ,2) 
  sc 12     = ("brk"                        ,1) 
  sc 13     = ("rt_sigaction"               ,4) 
  sc 14     = ("rt_sigprocmask"             ,4) 
  sc 15     = ("rt_sigreturn"               ,6) 
  sc 16     = ("ioctl"                      ,3) 
  sc 17     = ("pread64"                    ,4) 
  sc 18     = ("pwrite64"                   ,4) 
  sc 19     = ("readv"                      ,3) 
  sc 20     = ("writev"                     ,3) 
  sc 21     = ("access"                     ,2) 
  sc 22     = ("pipe"                       ,1) 
  sc 23     = ("select"                     ,5) 
  sc 24     = ("sched_yield"                ,0) 
  sc 25     = ("mremap"                     ,5) 
  sc 26     = ("msync"                      ,3) 
  sc 27     = ("mincore"                    ,3) 
  sc 28     = ("madvise"                    ,3) 
  sc 29     = ("shmget"                     ,3) 
  sc 30     = ("shmat"                      ,3) 
  sc 31     = ("shmctl"                     ,3) 
  sc 32     = ("dup"                        ,1) 
  sc 33     = ("dup2"                       ,2) 
  sc 34     = ("pause"                      ,0) 
  sc 35     = ("nanosleep"                  ,2) 
  sc 36     = ("getitimer"                  ,2) 
  sc 37     = ("alarm"                      ,1) 
  sc 38     = ("setitimer"                  ,3) 
  sc 39     = ("getpid"                     ,0) 
  sc 40     = ("sendfile"                   ,4) 
  sc 41     = ("socket"                     ,3) 
  sc 42     = ("connect"                    ,3) 
  sc 43     = ("accept"                     ,3) 
  sc 44     = ("sendto"                     ,6) 
  sc 45     = ("recvfrom"                   ,6) 
  sc 46     = ("sendmsg"                    ,3) 
  sc 47     = ("recvmsg"                    ,3) 
  sc 48     = ("shutdown"                   ,2) 
  sc 49     = ("bind"                       ,3) 
  sc 50     = ("listen"                     ,2) 
  sc 51     = ("getsockname"                ,3) 
  sc 52     = ("getpeername"                ,3) 
  sc 53     = ("socketpair"                 ,4) 
  sc 54     = ("setsockopt"                 ,5) 
  sc 55     = ("getsockopt"                 ,5) 
  sc 56     = ("clone"                      ,5) 
  sc 57     = ("fork"                       ,0) 
  sc 58     = ("vfork"                      ,0) 
  sc 59     = ("execve"                     ,3) 
  sc 60     = ("exit"                       ,1) 
  sc 61     = ("wait4"                      ,4) 
  sc 62     = ("kill"                       ,2) 
  sc 63     = ("uname"                      ,1) 
  sc 64     = ("semget"                     ,3) 
  sc 65     = ("semop"                      ,3) 
  sc 66     = ("semctl"                     ,4) 
  sc 67     = ("shmdt"                      ,1) 
  sc 68     = ("msgget"                     ,2) 
  sc 69     = ("msgsnd"                     ,4) 
  sc 70     = ("msgrcv"                     ,5) 
  sc 71     = ("msgctl"                     ,3) 
  sc 72     = ("fcntl"                      ,3) 
  sc 73     = ("flock"                      ,2) 
  sc 74     = ("fsync"                      ,1) 
  sc 75     = ("fdatasync"                  ,1) 
  sc 76     = ("truncate"                   ,2) 
  sc 77     = ("ftruncate"                  ,2) 
  sc 78     = ("getdents"                   ,3) 
  sc 79     = ("getcwd"                     ,2) 
  sc 80     = ("chdir"                      ,1) 
  sc 81     = ("fchdir"                     ,1) 
  sc 82     = ("rename"                     ,2) 
  sc 83     = ("mkdir"                      ,2) 
  sc 84     = ("rmdir"                      ,1) 
  sc 85     = ("creat"                      ,2) 
  sc 86     = ("link"                       ,2) 
  sc 87     = ("unlink"                     ,1) 
  sc 88     = ("symlink"                    ,2) 
  sc 89     = ("readlink"                   ,3) 
  sc 90     = ("chmod"                      ,2) 
  sc 91     = ("fchmod"                     ,2) 
  sc 92     = ("chown"                      ,3) 
  sc 93     = ("fchown"                     ,3) 
  sc 94     = ("lchown"                     ,3) 
  sc 95     = ("umask"                      ,1) 
  sc 96     = ("gettimeofday"               ,2) 
  sc 97     = ("getrlimit"                  ,2) 
  sc 98     = ("getrusage"                  ,2) 
  sc 99     = ("sysinfo"                    ,1) 
  sc 100    = ("times"                      ,1) 
  sc 101    = ("ptrace"                     ,4) 
  sc 102    = ("getuid"                     ,0) 
  sc 103    = ("syslog"                     ,3) 
  sc 104    = ("getgid"                     ,0) 
  sc 105    = ("setuid"                     ,1) 
  sc 106    = ("setgid"                     ,1) 
  sc 107    = ("geteuid"                    ,0) 
  sc 108    = ("getegid"                    ,0) 
  sc 109    = ("setpgid"                    ,2) 
  sc 110    = ("getppid"                    ,0) 
  sc 111    = ("getpgrp"                    ,0) 
  sc 112    = ("setsid"                     ,0) 
  sc 113    = ("setreuid"                   ,2) 
  sc 114    = ("setregid"                   ,2) 
  sc 115    = ("getgroups"                  ,2) 
  sc 116    = ("setgroups"                  ,2) 
  sc 117    = ("setresuid"                  ,3) 
  sc 118    = ("getresuid"                  ,3) 
  sc 119    = ("setresgid"                  ,3) 
  sc 120    = ("getresgid"                  ,3) 
  sc 121    = ("getpgid"                    ,1) 
  sc 122    = ("setfsuid"                   ,1) 
  sc 123    = ("setfsgid"                   ,1) 
  sc 124    = ("getsid"                     ,1) 
  sc 125    = ("capget"                     ,2) 
  sc 126    = ("capset"                     ,2) 
  sc 127    = ("rt_sigpending"              ,2) 
  sc 128    = ("rt_sigtimedwait"            ,4) 
  sc 129    = ("rt_sigqueueinfo"            ,3) 
  sc 130    = ("rt_sigsuspend"              ,2) 
  sc 131    = ("sigaltstack"                ,2) 
  sc 132    = ("utime"                      ,2) 
  sc 133    = ("mknod"                      ,3) 
  sc 134    = ("uselib"                     ,1) 
  sc 135    = ("personality"                ,1) 
  sc 136    = ("ustat"                      ,2) 
  sc 137    = ("statfs"                     ,2) 
  sc 138    = ("fstatfs"                    ,2) 
  sc 139    = ("sysfs"                      ,3) 
  sc 140    = ("getpriority"                ,2) 
  sc 141    = ("setpriority"                ,3) 
  sc 142    = ("sched_setparam"             ,2) 
  sc 143    = ("sched_getparam"             ,2) 
  sc 144    = ("sched_setscheduler"         ,3) 
  sc 145    = ("sched_getscheduler"         ,1) 
  sc 146    = ("sched_get_priority_max"     ,1) 
  sc 147    = ("sched_get_priority_min"     ,1) 
  sc 148    = ("sched_rr_get_interval"      ,2) 
  sc 149    = ("mlock"                      ,2) 
  sc 150    = ("munlock"                    ,2) 
  sc 151    = ("mlockall"                   ,1) 
  sc 152    = ("munlockall"                 ,0) 
  sc 153    = ("vhangup"                    ,0) 
  sc 154    = ("modify_ldt"                 ,6) 
  sc 155    = ("pivot_root"                 ,2) 
  sc 156    = ("_sysctl"                    ,6) 
  sc 157    = ("prctl"                      ,5) 
  sc 158    = ("arch_prctl"                 ,6) 
  sc 159    = ("adjtimex"                   ,1) 
  sc 160    = ("setrlimit"                  ,2) 
  sc 161    = ("chroot"                     ,1) 
  sc 162    = ("sync"                       ,0) 
  sc 163    = ("acct"                       ,1) 
  sc 164    = ("settimeofday"               ,2) 
  sc 165    = ("mount"                      ,5) 
  sc 166    = ("umount2"                    ,6) 
  sc 167    = ("swapon"                     ,2) 
  sc 168    = ("swapoff"                    ,1) 
  sc 169    = ("reboot"                     ,4) 
  sc 170    = ("sethostname"                ,2) 
  sc 171    = ("setdomainname"              ,2) 
  sc 172    = ("iopl"                       ,6) 
  sc 173    = ("ioperm"                     ,3) 
  sc 174    = ("create_module"              ,6) 
  sc 175    = ("init_module"                ,3) 
  sc 176    = ("delete_module"              ,2) 
  sc 177    = ("get_kernel_syms"            ,6) 
  sc 178    = ("query_module"               ,6) 
  sc 179    = ("quotactl"                   ,4) 
  sc 180    = ("nfsservctl"                 ,6) 
  sc 181    = ("getpmsg"                    ,6) 
  sc 182    = ("putpmsg"                    ,6) 
  sc 183    = ("afs_syscall"                ,6) 
  sc 184    = ("tuxcall"                    ,6) 
  sc 185    = ("security"                   ,6) 
  sc 186    = ("gettid"                     ,0) 
  sc 187    = ("readahead"                  ,3) 
  sc 188    = ("setxattr"                   ,5) 
  sc 189    = ("lsetxattr"                  ,5) 
  sc 190    = ("fsetxattr"                  ,5) 
  sc 191    = ("getxattr"                   ,4) 
  sc 192    = ("lgetxattr"                  ,4) 
  sc 193    = ("fgetxattr"                  ,4) 
  sc 194    = ("listxattr"                  ,3) 
  sc 195    = ("llistxattr"                 ,3) 
  sc 196    = ("flistxattr"                 ,3) 
  sc 197    = ("removexattr"                ,2) 
  sc 198    = ("lremovexattr"               ,2) 
  sc 199    = ("fremovexattr"               ,2) 
  sc 200    = ("tkill"                      ,2) 
  sc 201    = ("time"                       ,1) 
  sc 202    = ("futex"                      ,6) 
  sc 203    = ("sched_setaffinity"          ,3) 
  sc 204    = ("sched_getaffinity"          ,3) 
  sc 205    = ("set_thread_area"            ,6) 
  sc 206    = ("io_setup"                   ,2) 
  sc 207    = ("io_destroy"                 ,1) 
  sc 208    = ("io_getevents"               ,5) 
  sc 209    = ("io_submit"                  ,3) 
  sc 210    = ("io_cancel"                  ,3) 
  sc 211    = ("get_thread_area"            ,6) 
  sc 212    = ("lookup_dcookie"             ,6) 
  sc 213    = ("epoll_create"               ,1) 
  sc 214    = ("epoll_ctl_old"              ,6) 
  sc 215    = ("epoll_wait_old"             ,6) 
  sc 216    = ("remap_file_pages"           ,5) 
  sc 217    = ("getdents64"                 ,3) 
  sc 218    = ("set_tid_address"            ,1) 
  sc 219    = ("restart_syscall"            ,0) 
  sc 220    = ("semtimedop"                 ,4) 
  sc 221    = ("fadvise64"                  ,4) 
  sc 222    = ("timer_create"               ,3) 
  sc 223    = ("timer_settime"              ,4) 
  sc 224    = ("timer_gettime"              ,2) 
  sc 225    = ("timer_getoverrun"           ,1) 
  sc 226    = ("timer_delete"               ,1) 
  sc 227    = ("clock_settime"              ,2) 
  sc 228    = ("clock_gettime"              ,2) 
  sc 229    = ("clock_getres"               ,2) 
  sc 230    = ("clock_nanosleep"            ,4) 
  sc 231    = ("exit_group"                 ,1) 
  sc 232    = ("epoll_wait"                 ,4) 
  sc 233    = ("epoll_ctl"                  ,4) 
  sc 234    = ("tgkill"                     ,3) 
  sc 235    = ("utimes"                     ,2) 
  sc 236    = ("vserver"                    ,6) 
  sc 237    = ("mbind"                      ,6) 
  sc 238    = ("set_mempolicy"              ,3) 
  sc 239    = ("get_mempolicy"              ,5) 
  sc 240    = ("mq_open"                    ,4) 
  sc 241    = ("mq_unlink"                  ,1) 
  sc 242    = ("mq_timedsend"               ,5) 
  sc 243    = ("mq_timedreceive"            ,5) 
  sc 244    = ("mq_notify"                  ,2) 
  sc 245    = ("mq_getsetattr"              ,3) 
  sc 246    = ("kexec_load"                 ,4) 
  sc 247    = ("waitid"                     ,5) 
  sc 248    = ("add_key"                    ,5) 
  sc 249    = ("request_key"                ,4) 
  sc 250    = ("keyctl"                     ,5) 
  sc 251    = ("ioprio_set"                 ,3) 
  sc 252    = ("ioprio_get"                 ,2) 
  sc 253    = ("inotify_init"               ,0) 
  sc 254    = ("inotify_add_watch"          ,3) 
  sc 255    = ("inotify_rm_watch"           ,2) 
  sc 256    = ("migrate_pages"              ,4) 
  sc 257    = ("openat"                     ,4) 
  sc 258    = ("mkdirat"                    ,3) 
  sc 259    = ("mknodat"                    ,4) 
  sc 260    = ("fchownat"                   ,5) 
  sc 261    = ("futimesat"                  ,3) 
  sc 262    = ("newfstatat"                 ,4) 
  sc 263    = ("unlinkat"                   ,3) 
  sc 264    = ("renameat"                   ,4) 
  sc 265    = ("linkat"                     ,5) 
  sc 266    = ("symlinkat"                  ,3) 
  sc 267    = ("readlinkat"                 ,4) 
  sc 268    = ("fchmodat"                   ,3) 
  sc 269    = ("faccessat"                  ,3) 
  sc 270    = ("pselect6"                   ,6) 
  sc 271    = ("ppoll"                      ,5) 
  sc 272    = ("unshare"                    ,1) 
  sc 273    = ("set_robust_list"            ,2) 
  sc 274    = ("get_robust_list"            ,3) 
  sc 275    = ("splice"                     ,6) 
  sc 276    = ("tee"                        ,4) 
  sc 277    = ("sync_file_range"            ,4) 
  sc 278    = ("vmsplice"                   ,4) 
  sc 279    = ("move_pages"                 ,6) 
  sc 280    = ("utimensat"                  ,4) 
  sc 281    = ("epoll_pwait"                ,6) 
  sc 282    = ("signalfd"                   ,3) 
  sc 283    = ("timerfd_create"             ,2) 
  sc 284    = ("eventfd"                    ,1) 
  sc 285    = ("fallocate"                  ,4) 
  sc 286    = ("timerfd_settime"            ,4) 
  sc 287    = ("timerfd_gettime"            ,2) 
  sc 288    = ("accept4"                    ,4) 
  sc 289    = ("signalfd4"                  ,4) 
  sc 290    = ("eventfd2"                   ,2) 
  sc 291    = ("epoll_create1"              ,1) 
  sc 292    = ("dup3"                       ,3) 
  sc 293    = ("pipe2"                      ,2) 
  sc 294    = ("inotify_init1"              ,1) 
  sc 295    = ("preadv"                     ,5) 
  sc 296    = ("pwritev"                    ,5) 
  sc 297    = ("rt_tgsigqueueinfo"          ,4) 
  sc 298    = ("perf_event_open"            ,5) 
  sc 299    = ("recvmmsg"                   ,5) 
  sc 300    = ("fanotify_init"              ,2) 
  sc 301    = ("fanotify_mark"              ,5) 
  sc 302    = ("prlimit64"                  ,4) 
  sc 303    = ("name_to_handle_at"          ,5) 
  sc 304    = ("open_by_handle_at"          ,3) 
  sc 305    = ("clock_adjtime"              ,2) 
  sc 306    = ("syncfs"                     ,1) 
  sc 307    = ("sendmmsg"                   ,4) 
  sc 308    = ("setns"                      ,2) 
  sc 309    = ("getcpu"                     ,3) 
  sc 310    = ("process_vm_readv"           ,6) 
  sc 311    = ("process_vm_writev"          ,6) 
  sc 312    = ("kcmp"                       ,5) 
  sc 313    = ("finit_module"               ,3) 
  sc 314    = ("sched_setattr"              ,3) 
  sc 315    = ("sched_getattr"              ,4) 
  sc 316    = ("renameat2"                  ,5) 
  sc 317    = ("seccomp"                    ,3) 
  sc 318    = ("getrandom"                  ,3) 
  sc 319    = ("memfd_create"               ,2) 
  sc 320    = ("kexec_file_load"            ,5) 
  sc 321    = ("bpf"                        ,3) 
  sc 322    = ("execveat"                   ,5) 
  sc 323    = ("userfaultfd"                ,1) 
  sc 324    = ("membarrier"                 ,3) 
  sc 325    = ("mlock2"                     ,3) 
  sc 326    = ("copy_file_range"            ,6) 
  sc 327    = ("preadv2"                    ,6) 
  sc 328    = ("pwritev2"                   ,6) 
  sc 329    = ("pkey_mprotect"              ,4) 
  sc 330    = ("pkey_alloc"                 ,2) 
  sc 331    = ("pkey_free"                  ,1) 
  sc 332    = ("statx"                      ,5) 
  sc 333    = ("io_pgetevents"              ,6) 
  sc 334    = ("rseq"                       ,4) 
  sc 424    = ("pidfd_send_signal"          ,4) 
  sc 425    = ("io_uring_setup"             ,2) 
  sc 426    = ("io_uring_enter"             ,6) 
  sc 427    = ("io_uring_register"          ,4) 
  sc 428    = ("open_tree"                  ,3) 
  sc 429    = ("move_mount"                 ,5) 
  sc 430    = ("fsopen"                     ,2) 
  sc 431    = ("fsconfig"                   ,5) 
  sc 432    = ("fsmount"                    ,3) 
  sc 433    = ("fspick"                     ,3) 
  sc 434    = ("pidfd_open"                 ,2) 
  sc 435    = ("clone3"                     ,2) 
  sc 436    = ("close_range"                ,3) 
  sc 439    = ("faccessat2"                 ,4) 
  sc 441    = ("epoll_pwait2", 6)
  sc 442    = ("mount_setattr", 5)    
  sc 443    = ("quotactl_fd", 4)    
  sc 444    = ("landlock_create_ruleset", 3)   
  sc 445    = ("landlock_add_rule", 4)    
  sc 446    = ("landlock_restrict_self", 2)    
  sc 447    = ("memfd_secret", 1)    
  sc 448    = ("process_mrelease", 2)    
  sc 449    = ("futex_waitv", 5)    
  sc 450    = ("set_mempolicy_home_node", 4)    
  sc 451    = ("cachestat", 4)    
  sc 452    = ("fchmodat2", 4)    
  sc 453    = ("map_shadow_stack", 3)    
  sc 454    = ("futex_wake", 4)    
  sc 455    = ("futex_wait", 6)    
  sc 456    = ("futex_requeue", 4)    
  sc 457    = ("statmount", 4)    
  sc 458    = ("listmount", 4)    
  sc 459    = ("lsm_get_self_attr", 4)    
  sc 460    = ("lsm_set_self_attr", 4)    
  sc 461    = ("lsm_list_modules", 3)    
  sc 462    = ("mseal", 3)    
  sc 463    = ("setxattrat", 6)    
  sc 464    = ("getxattrat", 6)    
  sc 465    = ("listxattrat", 5)    
  sc 466    = ("removexattrat", 4)    
  sc 467    = ("open_tree_attr", 5)
  sc n      = error $ "Unknown syscall: " ++ show n
                                                   
