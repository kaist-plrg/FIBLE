#ifndef __APPLE__
#include <asm/termbits.h>
#include <asm/unistd.h>
#endif
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#ifdef __APPLE__
#define DO_SYS(sname, ...) caml_failwith("syscall " #sname " not supported on macOS")
#else
#define DO_SYS(sname, ...) do { \
  long retv = syscall(__NR_ ## sname, ##__VA_ARGS__); \
  if (retv == -1) CAMLreturn(caml_copy_int64((long)-errno)); \
  CAMLreturn(caml_copy_int64(retv)); \
} while (0)
#endif

#define __NR_faccessat2 439
#define __NR_fchmodat2 452

// 0: read
CAMLprim value unix_read (value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (read, Int_val(fd), Bytes_val(buf), Int64_val(len));
}

// 1: write
CAMLprim value unix_write(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (write, Int_val(fd), String_val(buf), Int64_val(len));
}

// 2: open
CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  DO_SYS (open, String_val(path), Int_val(flags), Int_val(perm));
}

// 3: close
CAMLprim value unix_close(value fd)
{
  CAMLparam1(fd);
  DO_SYS (close, Int_val(fd));
}

// 4: stat
CAMLprim value unix_stat(value path, value buf)
{
  CAMLparam2(path, buf);
  DO_SYS (stat, String_val(path), (struct stat *)Bytes_val(buf));
}

// 5: fstat
CAMLprim value unix_fstat(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (fstat, Int_val(fd), (struct stat *)Bytes_val(buf));
}

// 6: lstat
CAMLprim value unix_lstat(value path, value buf)
{
  CAMLparam2(path, buf);
  DO_SYS (lstat, String_val(path), (struct stat *)Bytes_val(buf));
}

// 7: lseek
CAMLprim value unix_lseek(value fd, value offset, value whence)
{
  CAMLparam3(fd, offset, whence);
  DO_SYS (lseek, Int_val(fd), Int64_val(offset), Int_val(whence));
}

// 16-1: ioctl-tcgets
CAMLprim value unix_tcgets(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCGETS, (struct termios *)Bytes_val(buf));
}

// 16-2: ioctl-tcsets
CAMLprim value unix_tcsets(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCSETS, (const struct termios *)String_val(buf));
}

// 16-3: ioctl-tcsetsw
CAMLprim value unix_tcsetsw(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCSETSW, (const struct termios *)String_val(buf));
}

// 16-4: ioctl-tiocgwinsz
CAMLprim value unix_tiocgwinsz(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TIOCGWINSZ, (struct winsize *)Bytes_val(buf));
}

// 16-5: ioctl-fionread
CAMLprim value unix_fionread(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), FIONREAD, (int *)Bytes_val(buf));
}

// 16-6: ioctl-btrfs_ioc_clone
# define BTRFS_IOC_CLONE 1074041865
CAMLprim value unix_btrfs_ioc_clone(value destfd, value srcfd)
{
  CAMLparam2(destfd, srcfd);
  DO_SYS (ioctl, Int_val(destfd), BTRFS_IOC_CLONE, Int_val(srcfd));
}

// 33: dup2
CAMLprim value unix_dup2(value oldfd, value newfd)
{
  CAMLparam2(oldfd, newfd);
  DO_SYS (dup2, Int_val(oldfd), Int_val(newfd));
}

// 41: socket
CAMLprim value unix_socket(value domain, value type, value protocol)
{
  CAMLparam3(domain, type, protocol);
  DO_SYS (socket, Int_val(domain), Int_val(type), Int_val(protocol));
}

// 42: connect
CAMLprim value unix_connect(value fd, value addr, value addrlen)
{
  CAMLparam3(fd, addr, addrlen);
  DO_SYS (connect, Int_val(fd), (struct sockaddr *)Bytes_val(addr), Int_val(addrlen));
}

// 63: uname
CAMLprim value unix_uname(value buf)
{
  CAMLparam1(buf);
  DO_SYS (uname, (struct utsname *)Bytes_val(buf));
}

// 72-1: fcntl-f_dupfd
CAMLprim value unix_dupfd(value fd, value minfd)
{
  CAMLparam2(fd, minfd);
  DO_SYS (fcntl, Int_val(fd), F_DUPFD, Int_val(minfd));
}

// 72-2: fcntl-f_getfl
CAMLprim value unix_getfl(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETFL);
}

// 72-3: fcntl-f_setfl
CAMLprim value unix_setfl(value fd, value flags)
{
  CAMLparam2(fd, flags);
  DO_SYS (fcntl, Int_val(fd), F_SETFL, Int_val(flags));
}

// 72-4: fcntl-f_getfd
CAMLprim value unix_getfd(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETFD);
}

// 72-5: fcntl-f_setfd
CAMLprim value unix_setfd(value fd, value flags)
{
  CAMLparam2(fd, flags);
  DO_SYS (fcntl, Int_val(fd), F_SETFD, Int_val(flags));
}

// 72-6: fcntl-f_getown
CAMLprim value unix_getown(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETOWN);
}

// 72-7: fcntl-f_setown
CAMLprim value unix_setown(value fd, value pid)
{
  CAMLparam2(fd, pid);
  DO_SYS (fcntl, Int_val(fd), F_SETOWN, Int_val(pid));
}

// 72-8: fcntl-f_dupfd_cloexec
CAMLprim value unix_dupfd_cloexec(value fd, value minfd)
{
  CAMLparam2(fd, minfd);
  DO_SYS (fcntl, Int_val(fd), F_DUPFD_CLOEXEC, Int_val(minfd));
}

// 75: fdatasync
CAMLprim value unix_fdatasync(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fdatasync, Int_val(fd));
}

// 77: ftruncate
CAMLprim value unix_ftruncate(value fd, value length)
{
  CAMLparam2(fd, length);
  DO_SYS (ftruncate, Int_val(fd), Int64_val(length));
}

// 79: getcwd
CAMLprim value unix_getcwd(value buf, value len)
{
  CAMLparam2(buf, len);
  DO_SYS (getcwd, Bytes_val(buf), Int_val(len));
}

// 80: chdir
CAMLprim value unix_chdir(value path)
{
  CAMLparam1(path);
  DO_SYS (chdir, String_val(path));
}

// 81: fchdir
CAMLprim value unix_fchdir(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fchdir, Int_val(fd));
}

// 84: rmdir
CAMLprim value unix_rmdir(value path)
{
  CAMLparam1(path);
  DO_SYS (rmdir, String_val(path));
}

// 87: unlink
CAMLprim value unix_unlink(value path)
{
  CAMLparam1(path);
  DO_SYS (unlink, String_val(path));
}

// 89: readlink
CAMLprim value unix_readlink(value path, value buf, value len)
{
  CAMLparam3(path, buf, len);
  DO_SYS (readlink, String_val(path), Bytes_val(buf), Int_val(len));
}

// 90: chmod
CAMLprim value unix_chmod(value path, value perm)
{
  CAMLparam2(path, perm);
  DO_SYS (chmod, String_val(path), Int_val(perm));
}

// 91: fchmod
CAMLprim value unix_fchmod(value fd, value perm)
{
  CAMLparam2(fd, perm);
  DO_SYS (fchmod, Int_val(fd), Int_val(perm));
}

// 93: fchown
CAMLprim value unix_fchown(value fd, value uid, value gid)
{
  CAMLparam3(fd, uid, gid);
  DO_SYS (fchown, Int_val(fd), Int_val(uid), Int_val(gid));
}

// 95: umask
CAMLprim value unix_umask(value mask)
{
  CAMLparam1(mask);
  DO_SYS (umask, Int_val(mask));
}

// 96: gettimeofday
CAMLprim value unix_gettimeofday(value tv, value tz)
{
  CAMLparam2(tv, tz);
  DO_SYS (gettimeofday, (struct timeval *)Bytes_val(tv), (struct timezone *)Bytes_val(tz));
}

// 99: sysinfo
CAMLprim value unix_sysinfo(value buf)
{
  CAMLparam1(buf);
  DO_SYS (sysinfo, (struct sysinfo *)Bytes_val(buf));
}

// 115: getgroups
CAMLprim value unix_getgroups(value size, value buf)
{
  CAMLparam2(size, buf);
  DO_SYS (getgroups, Int_val(size), (gid_t *)Bytes_val(buf));
}

// 115-1: getgroups_null
CAMLprim value unix_getgroups_null(value size)
{
  CAMLparam1(size);
  DO_SYS (getgroups, Int_val(size), NULL);
}

// 137: statfs
CAMLprim value unix_statfs(value path, value buf)
{
  CAMLparam2(path, buf);
  DO_SYS (statfs, String_val(path), (struct statfs *)Bytes_val(buf));
}

// 138: fstatfs
CAMLprim value unix_fstatfs(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (fstatfs, Int_val(fd), (struct statfs *)Bytes_val(buf));
}

// 161: chroot
CAMLprim value unix_chroot(value path)
{
  CAMLparam1(path);
  DO_SYS (chroot, String_val(path));
}

// 217: getdents64
CAMLprim value unix_getdents64(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (getdents64, Int_val(fd), Bytes_val(buf), Int_val(len));
}

// 221: fadvise64
CAMLprim value unix_fadvise64(value fd, value offset, value len, value advice)
{
  CAMLparam4(fd, offset, len, advice);
  DO_SYS (fadvise64, Int_val(fd), Int64_val(offset), Int64_val(len), Int_val(advice));
}

// 228: clock_gettime
CAMLprim value unix_clock_gettime(value clk_id, value tp)
{
  CAMLparam2(clk_id, tp);
  DO_SYS (clock_gettime, Int64_val(clk_id), (struct timespec *)Bytes_val(tp));
}

// 257: openat
CAMLprim value unix_openat(value dirfd, value path, value flags, value perm)
{
  CAMLparam4(dirfd, path, flags, perm);
  DO_SYS (openat, Int_val(dirfd), String_val(path), Int_val(flags), Int_val(perm));
}

// 258: mkdirat
CAMLprim value unix_mkdirat(value dirfd, value path, value perm)
{
  CAMLparam3(dirfd, path, perm);
  DO_SYS (mkdirat, Int_val(dirfd), String_val(path), Int_val(perm));
}

// 259: mknodat
CAMLprim value unix_mknodat(value dirfd, value path, value mode, value dev)
{
  CAMLparam4(dirfd, path, mode, dev);
  DO_SYS (mknodat, Int_val(dirfd), String_val(path), Int_val(mode), Int_val(dev));
}

// 260: fchownat
CAMLprim value unix_fchownat(value dirfd, value path, value uid, value gid, value flags)
{
  CAMLparam5(dirfd, path, uid, gid, flags);
  DO_SYS (fchownat, Int_val(dirfd), String_val(path), Int_val(uid), Int_val(gid), Int_val(flags));
}

// 262: newfstatat
CAMLprim value unix_newfstatat(value dirfd, value path, value buf, value flags)
{
  CAMLparam4(dirfd, path, buf, flags);
  DO_SYS (newfstatat, Int_val(dirfd), String_val(path), (struct stat *)Bytes_val(buf), Int_val(flags));
}

// 263: unlinkat
CAMLprim value unix_unlinkat(value dirfd, value path, value flags)
{
  CAMLparam3(dirfd, path, flags);
  DO_SYS (unlinkat, Int_val(dirfd), String_val(path), Int_val(flags));
}

// 264: renameat
CAMLprim value unix_renameat(value oldfd, value oldpath, value newfd, value newpath)
{
  CAMLparam4(oldfd, oldpath, newfd, newpath);
  DO_SYS (renameat, Int_val(oldfd), String_val(oldpath), Int_val(newfd), String_val(newpath));
}

// 265: linkat
CAMLprim value unix_linkat(value oldfd, value oldpath, value newfd, value newpath, value flags)
{
  CAMLparam5(oldfd, oldpath, newfd, newpath, flags);
  DO_SYS (linkat, Int_val(oldfd), String_val(oldpath), Int_val(newfd), String_val(newpath), Int_val(flags));
}

// 266: symlinkat
CAMLprim value unix_symlinkat(value oldpath, value newfd, value newpath)
{
  CAMLparam3(oldpath, newfd, newpath);
  DO_SYS (symlinkat, String_val(oldpath), Int_val(newfd), String_val(newpath));
}

// 267: readlinkat
CAMLprim value unix_readlinkat(value fd, value path, value buf, value bufsize)
{
  CAMLparam4(fd, path, buf, bufsize);
  DO_SYS (readlinkat, Int_val(fd), String_val(path), Bytes_val(buf), Int_val(bufsize));
}

// 268: fchmodat
CAMLprim value unix_fchmodat(value dirfd, value path, value mode)
{
  CAMLparam3(dirfd, path, mode);
  DO_SYS (fchmodat, Int_val(dirfd), String_val(path), Int_val(mode));
}

// 269: faccessat
CAMLprim value unix_faccessat(value dirfd, value path, value mode)
{
  CAMLparam3(dirfd, path, mode);
  DO_SYS (faccessat, Int_val(dirfd), String_val(path), Int_val(mode));
}

// 280: utimensat
CAMLprim value unix_utimensat(value dirfd, value path, value times, value flags)
{
  CAMLparam4(dirfd, path, times, flags);
  DO_SYS (utimensat, Int_val(dirfd), String_val(path), (const struct timespec *)Bytes_val(times), Int_val(flags));
}

// 280-1: utimensat-pathnull
CAMLprim value unix_utimensat_pathnull(value dirfd, value times, value flags)
{
  CAMLparam3(dirfd, times, flags);
  DO_SYS (utimensat, Int_val(dirfd), NULL, (const struct timespec *)Bytes_val(times), Int_val(flags));
}

// 285: fallocate
CAMLprim value unix_fallocate(value fd, value mode, value offset, value len)
{
  CAMLparam4(fd, mode, offset, len);
  DO_SYS (fallocate, Int_val(fd), Int_val(mode), Int64_val(offset), Int64_val(len));
}

// 316: renameat2
CAMLprim value unix_renameat2(value oldfd, value oldpath, value newfd, value newpath, value flags)
{
  CAMLparam5(oldfd, oldpath, newfd, newpath, flags);
  DO_SYS (renameat2, Int_val(oldfd), String_val(oldpath), Int_val(newfd), String_val(newpath), Int_val(flags));
}

// 318: getrandom
CAMLprim value unix_getrandom(value buf, value len, value flags)
{
  CAMLparam3(buf, len, flags);
  DO_SYS (getrandom, Bytes_val(buf), Int64_val(len), Int_val(flags));
}

// 332: statx
CAMLprim value unix_statx(value dirfd, value path, value flags, value mask, value buf)
{
  CAMLparam5(dirfd, path, flags, mask, buf);
  DO_SYS (statx, Int_val(dirfd), String_val(path), Int_val(flags), Int_val(mask), (struct statx *)Bytes_val(buf));
}

// 439: faccessat2
CAMLprim value unix_faccessat2(value dirfd, value path, value mode, value flags)
{
  CAMLparam4(dirfd, path, mode, flags);
  DO_SYS (faccessat2, Int_val(dirfd), String_val(path), Int_val(mode), Int_val(flags));
}

// 452: fchmodat2
CAMLprim value unix_fchmodat2(value dirfd, value path, value mode, value flags)
{
  CAMLparam4(dirfd, path, mode, flags);
  DO_SYS (fchmodat2, Int_val(dirfd), String_val(path), Int_val(mode), Int_val(flags));
}


CAMLprim value unix_fd_is_valid (value fd)
{
  CAMLparam1(fd);
  CAMLreturn(Val_bool(fcntl(Int_val(fd), F_GETFD) != -1));
}
