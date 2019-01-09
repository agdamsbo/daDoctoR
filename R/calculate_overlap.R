#' Calculating overlap from list of identifier numbers
#'
#' Ex for creating euler diagram using the euler() function of the eulerr package.
#' @param x Date of birth.
#' @keywords overlap
#' @export
#' @examples
#'

calculate_overlap<-function (x)
{
  ## Based on the calculate.overlap() function from the VennDiagram package, and extended for direct use with the euler() funtion from the eulerr package.
  if (1 == length(x)) {
    overlap <- x
  }
  else if (2 == length(x)) {
    overlap <- list(a1 = x[[1]], a2 = x[[2]], a3 = intersect(x[[1]],
                                                             x[[2]]))
  }
  else if (3 == length(x)) {
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    nab <- intersect(A, B)
    nbc <- intersect(B, C)
    nac <- intersect(A, C)
    nabc <- intersect(nab, C)
    a5 = nabc
    a2 = nab[which(!nab %in% a5)]
    a4 = nac[which(!nac %in% a5)]
    a6 = nbc[which(!nbc %in% a5)]
    a1 = A[which(!A %in% c(a2, a4, a5))]
    a3 = B[which(!B %in% c(a2, a5, a6))]
    a7 = C[which(!C %in% c(a4, a5, a6))]
    overlap <- list(a5 = a5, a2 = a2, a4 = a4, a6 = a6,
                    a1 = a1, a3 = a3, a7 = a7,A=A,B=B,C=C)
  }
  else if (4 == length(x)) {
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    D <- x[[4]]
    n12 <- intersect(A, B)
    n13 <- intersect(A, C)
    n14 <- intersect(A, D)
    n23 <- intersect(B, C)
    n24 <- intersect(B, D)
    n34 <- intersect(C, D)
    n123 <- intersect(n12, C)
    n124 <- intersect(n12, D)
    n134 <- intersect(n13, D)
    n234 <- intersect(n23, D)
    n1234 <- intersect(n123, D)
    a6 = n1234
    a12 = n123[which(!n123 %in% a6)]
    a11 = n124[which(!n124 %in% a6)]
    a5 = n134[which(!n134 %in% a6)]
    a7 = n234[which(!n234 %in% a6)]
    a15 = n12[which(!n12 %in% c(a6, a11, a12))]
    a4 = n13[which(!n13 %in% c(a6, a5, a12))]
    a10 = n14[which(!n14 %in% c(a6, a5, a11))]
    a13 = n23[which(!n23 %in% c(a6, a7, a12))]
    a8 = n24[which(!n24 %in% c(a6, a7, a11))]
    a2 = n34[which(!n34 %in% c(a6, a5, a7))]
    a9 = A[which(!A %in% c(a4, a5, a6, a10, a11, a12, a15))]
    a14 = B[which(!B %in% c(a6, a7, a8, a11, a12, a13, a15))]
    a1 = C[which(!C %in% c(a2, a4, a5, a6, a7, a12, a13))]
    a3 = D[which(!D %in% c(a2, a5, a6, a7, a8, a10, a11))]
    overlap <- list(a6 = a6, a12 = a12, a11 = a11, a5 = a5,
                    a7 = a7, a15 = a15, a4 = a4, a10 = a10, a13 = a13,
                    a8 = a8, a2 = a2, a9 = a9, a14 = a14, a1 = a1, a3 = a3,A=A,B=B,C=C,D=D)
  }
  else if (5 == length(x)) {
    A <- x[[1]]
    B <- x[[2]]
    C <- x[[3]]
    D <- x[[4]]
    E <- x[[5]]
    n12 <- intersect(A, B)
    n13 <- intersect(A, C)
    n14 <- intersect(A, D)
    n15 <- intersect(A, E)
    n23 <- intersect(B, C)
    n24 <- intersect(B, D)
    n25 <- intersect(B, E)
    n34 <- intersect(C, D)
    n35 <- intersect(C, E)
    n45 <- intersect(D, E)
    n123 <- intersect(n12, C)
    n124 <- intersect(n12, D)
    n125 <- intersect(n12, E)
    n134 <- intersect(n13, D)
    n135 <- intersect(n13, E)
    n145 <- intersect(n14, E)
    n234 <- intersect(n23, D)
    n235 <- intersect(n23, E)
    n245 <- intersect(n24, E)
    n345 <- intersect(n34, E)
    n1234 <- intersect(n123, D)
    n1235 <- intersect(n123, E)
    n1245 <- intersect(n124, E)
    n1345 <- intersect(n134, E)
    n2345 <- intersect(n234, E)
    n12345 <- intersect(n1234, E)
    a31 = n12345
    a30 = n1234[which(!n1234 %in% a31)]
    a29 = n1235[which(!n1235 %in% a31)]
    a28 = n1245[which(!n1245 %in% a31)]
    a27 = n1345[which(!n1345 %in% a31)]
    a26 = n2345[which(!n2345 %in% a31)]
    a25 = n245[which(!n245 %in% c(a26, a28, a31))]
    a24 = n234[which(!n234 %in% c(a26, a30, a31))]
    a23 = n134[which(!n134 %in% c(a27, a30, a31))]
    a22 = n123[which(!n123 %in% c(a29, a30, a31))]
    a21 = n235[which(!n235 %in% c(a26, a29, a31))]
    a20 = n125[which(!n125 %in% c(a28, a29, a31))]
    a19 = n124[which(!n124 %in% c(a28, a30, a31))]
    a18 = n145[which(!n145 %in% c(a27, a28, a31))]
    a17 = n135[which(!n135 %in% c(a27, a29, a31))]
    a16 = n345[which(!n345 %in% c(a26, a27, a31))]
    a15 = n45[which(!n45 %in% c(a18, a25, a16, a28, a27,
                                a26, a31))]
    a14 = n24[which(!n24 %in% c(a19, a24, a25, a30, a28,
                                a26, a31))]
    a13 = n34[which(!n34 %in% c(a16, a23, a24, a26, a27,
                                a30, a31))]
    a12 = n13[which(!n13 %in% c(a17, a22, a23, a27, a29,
                                a30, a31))]
    a11 = n23[which(!n23 %in% c(a21, a22, a24, a26, a29,
                                a30, a31))]
    a10 = n25[which(!n25 %in% c(a20, a21, a25, a26, a28,
                                a29, a31))]
    a9 = n12[which(!n12 %in% c(a19, a20, a22, a28, a29,
                               a30, a31))]
    a8 = n14[which(!n14 %in% c(a18, a19, a23, a27, a28,
                               a30, a31))]
    a7 = n15[which(!n15 %in% c(a17, a18, a20, a27, a28,
                               a29, a31))]
    a6 = n35[which(!n35 %in% c(a16, a17, a21, a26, a27,
                               a29, a31))]
    a5 = E[which(!E %in% c(a6, a7, a15, a16, a17, a18, a25,
                           a26, a27, a28, a31, a20, a29, a21, a10))]
    a4 = D[which(!D %in% c(a13, a14, a15, a16, a23, a24,
                           a25, a26, a27, a28, a31, a18, a19, a8, a30))]
    a3 = C[which(!C %in% c(a21, a11, a12, a13, a29, a22,
                           a23, a24, a30, a31, a26, a27, a16, a6, a17))]
    a2 = B[which(!B %in% c(a9, a10, a19, a20, a21, a11,
                           a28, a29, a31, a22, a30, a26, a25, a24, a14))]
    a1 = A[which(!A %in% c(a7, a8, a18, a17, a19, a9, a27,
                           a28, a31, a20, a30, a29, a22, a23, a12))]
    overlap <- list(a31 = a31, a30 = a30, a29 = a29, a28 = a28,
                    a27 = a27, a26 = a26, a25 = a25, a24 = a24, a23 = a23,
                    a22 = a22, a21 = a21, a20 = a20, a19 = a19, a18 = a18,
                    a17 = a17, a16 = a16, a15 = a15, a14 = a14, a13 = a13,
                    a12 = a12, a11 = a11, a10 = a10, a9 = a9, a8 = a8,
                    a7 = a7, a6 = a6, a5 = a5, a4 = a4, a3 = a3, a2 = a2,
                    a1 = a1,A=A,B=B,C=C,D=D,E=E)
  }
  else {
    flog.error("Invalid size of input object", name = "VennDiagramLogger")
    stop("Invalid size of input object")
  }
  return(overlap)
}

