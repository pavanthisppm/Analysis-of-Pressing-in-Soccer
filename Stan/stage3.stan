data {
  int<lower=1> K;        
  array[K] int Y_A;      
  array[K] int Y_B;
  array[K] int Y_D;
  array[K] int Y_E;
}

parameters {
  simplex[K] theta_A;
  simplex[K] theta_B;
  simplex[K] theta_D;
  simplex[K] theta_E;
}

model {
  theta_A ~ dirichlet(rep_vector(1.0, K));
  theta_B ~ dirichlet(rep_vector(1.0, K));
  theta_D ~ dirichlet(rep_vector(1.0, K));
  theta_E ~ dirichlet(rep_vector(1.0, K));
 
  Y_A ~ multinomial(theta_A);
  Y_B ~ multinomial(theta_B);
  Y_D ~ multinomial(theta_D);
  Y_E ~ multinomial(theta_E);

  real gap = 0.05; 
  real lambda =  10000.0 ; 

  target += -lambda * square(fmax(0, gap - (theta_A[3] - theta_A[2])));
  target += -lambda * square(fmax(0, gap - (theta_A[4] - theta_A[2])));
  target += -lambda * square(fmax(0, gap - (theta_A[3] - theta_A[4])));

  // --- B-Origin ---
  target += -lambda * square(fmax(0, gap - (theta_B[1] - theta_B[3])));
  target += -lambda * square(fmax(0, gap - (theta_B[4] - theta_B[3])));
  //target += -lambda * square(fmax(0, gap - (theta_B[4] - theta_B[1])));

  // --- D-Origin ---
  target += -lambda * square(fmax(0, gap - (theta_D[1] - theta_D[2])));
  target += -lambda * square(fmax(0, gap - (theta_D[4] - theta_D[2])));
  //target += -lambda * square(fmax(0, gap - (theta_D[4] - theta_D[1])));

  // --- E-Origin ---
  target += -lambda * square(fmax(0, gap - (theta_E[3] - theta_E[1])));
  target += -lambda * square(fmax(0, gap - (theta_E[3] - theta_E[2])));
  target += -lambda * square(fmax(0, gap - (theta_E[2] - theta_E[1])));

  // --- Cross-origin: Index 6 ---
  target += -lambda * square(fmax(0, gap - (theta_A[6] - theta_B[6])));
  target += -lambda * square(fmax(0, gap - (theta_D[6] - theta_B[6])));
  target += -lambda * square(fmax(0, gap - (theta_D[6] - theta_E[6])));
  target += -lambda * square(fmax(0, gap - (theta_A[6] - theta_E[6])));
  target += -lambda * square(fmax(0, gap - (theta_B[6] - theta_E[6])));

  // --- Cross-origin: Dominant Nodes ---
  target += -lambda * square(fmax(0, gap - (theta_E[4] - theta_D[3])));
  target += -lambda * square(fmax(0, gap - (theta_B[2] - theta_A[1])));
  target += -lambda * square(fmax(0, gap - (theta_A[1] - theta_D[3])));
  target += -lambda * square(fmax(0, gap - (theta_E[4] - theta_B[2])));

  // --- Index 5 and others ---
  target += -lambda * square(fmax(0, gap - (theta_D[5] - theta_A[5])));
  target += -lambda * square(fmax(0, gap - (theta_E[5] - theta_B[5])));
  target += -lambda * square(fmax(0, gap - (theta_E[5] - theta_A[5])));
  target += -lambda * square(fmax(0, gap - (theta_D[5] - theta_B[5])));
  target += -lambda * square(fmax(0, gap - (theta_E[5] - theta_B[7])));
  //target += -lambda * square(fmax(0, gap - (theta_B[3] - theta_B[6])));
}
