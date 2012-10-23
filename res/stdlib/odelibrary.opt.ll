; ModuleID = 'odelibrary.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@is_first_run = global i8 1, align 1
@.str2 = private unnamed_addr constant [39 x i8] c"Starting simluation with output to %s\0A\00", align 1
@.str3 = private unnamed_addr constant [3 x i8] c"wb\00", align 1
@out_file = internal unnamed_addr global %struct._IO_FILE* null, align 8
@str = private unnamed_addr constant [33 x i8] c"Initialising the ODE environment\00"
@str6 = private unnamed_addr constant [34 x i8] c"Shutting down the ODE environment\00"
@str7 = private unnamed_addr constant [20 x i8] c"Finished simulation\00"
@str8 = private unnamed_addr constant [20 x i8] c"First run called...\00"

define void @init() nounwind uwtable {
entry:
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([33 x i8]* @str, i64 0, i64 0))
  ret void
}

declare i32 @printf(i8* nocapture, ...) nounwind

define void @shutdown() nounwind uwtable {
entry:
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([34 x i8]* @str6, i64 0, i64 0))
  ret void
}

define void @start_sim(i8* %filename) nounwind uwtable {
entry:
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([39 x i8]* @.str2, i64 0, i64 0), i8* %filename) nounwind
  %call1 = tail call %struct._IO_FILE* @fopen(i8* %filename, i8* getelementptr inbounds ([3 x i8]* @.str3, i64 0, i64 0)) nounwind
  store %struct._IO_FILE* %call1, %struct._IO_FILE** @out_file, align 8, !tbaa !0
  ret void
}

declare noalias %struct._IO_FILE* @fopen(i8* nocapture, i8* nocapture) nounwind

define void @end_sim() nounwind uwtable {
entry:
  %0 = load %struct._IO_FILE** @out_file, align 8, !tbaa !0
  %call = tail call i32 @fclose(%struct._IO_FILE* %0) nounwind
  store i8 1, i8* @is_first_run, align 1, !tbaa !3
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([20 x i8]* @str7, i64 0, i64 0))
  ret void
}

declare i32 @fclose(%struct._IO_FILE* nocapture) nounwind

define fastcc void @first_run(i32 %n_args) nounwind uwtable {
entry:
  %n_args.addr = alloca i32, align 4
  store i32 %n_args, i32* %n_args.addr, align 4, !tbaa !4
  store i8 0, i8* @is_first_run, align 1, !tbaa !3
  %puts = call i32 @puts(i8* getelementptr inbounds ([20 x i8]* @str8, i64 0, i64 0))
  %0 = bitcast i32* %n_args.addr to i8*
  %1 = load %struct._IO_FILE** @out_file, align 8, !tbaa !0
  %call1 = call i64 @fwrite(i8* %0, i64 4, i64 1, %struct._IO_FILE* %1) nounwind
  ret void
}

declare i64 @fwrite(i8* nocapture, i64, i64, %struct._IO_FILE* nocapture) nounwind

define fastcc void @write_dbls(i32 %n_args, double* nocapture noalias %dbls) nounwind uwtable {
entry:
  %n_args.addr.i = alloca i32, align 4
  %0 = load i8* @is_first_run, align 1, !tbaa !3, !range !5
  %tobool = icmp eq i8 %0, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %entry
  %1 = bitcast i32* %n_args.addr.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %1) nounwind
  store i32 %n_args, i32* %n_args.addr.i, align 4, !tbaa !4
  store i8 0, i8* @is_first_run, align 1, !tbaa !3
  %puts.i = call i32 @puts(i8* getelementptr inbounds ([20 x i8]* @str8, i64 0, i64 0)) nounwind
  %2 = load %struct._IO_FILE** @out_file, align 8, !tbaa !0
  %call1.i = call i64 @fwrite(i8* %1, i64 4, i64 1, %struct._IO_FILE* %2) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %1) nounwind
  br label %if.end

if.end:                                           ; preds = %entry, %if.then
  %3 = bitcast double* %dbls to i8*
  %conv = zext i32 %n_args to i64
  %4 = load %struct._IO_FILE** @out_file, align 8, !tbaa !0
  %call = call i64 @fwrite(i8* %3, i64 8, i64 %conv, %struct._IO_FILE* %4) nounwind
  ret void
}

define i32 @main() noreturn nounwind uwtable {
entry:
  tail call void @modelSolver() nounwind
  tail call void @exit(i32 0) noreturn nounwind
  unreachable
}

declare void @modelSolver()

declare void @exit(i32) noreturn nounwind

declare i32 @puts(i8* nocapture) nounwind

declare void @llvm.lifetime.start(i64, i8* nocapture) nounwind

declare void @llvm.lifetime.end(i64, i8* nocapture) nounwind

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"_Bool", metadata !1}
!4 = metadata !{metadata !"int", metadata !1}
!5 = metadata !{i8 0, i8 2}
