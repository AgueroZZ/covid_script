#include <TMB.hpp>                                // Links in the TMB libraries
//#include <fenv.h>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y); //response variable
  DATA_SPARSE_MATRIX(R); // Aggregation matrix
  DATA_SPARSE_MATRIX(B1); // Design matrix (for random effects 1: the yearly trend IWP)
  DATA_SPARSE_MATRIX(X1); // Design matrix (for boundary effects of IWP)
  DATA_SPARSE_MATRIX(P1); // Penalty matrix for IWP
  DATA_SCALAR(logP1det); // Determinant of (fixed) penalty matrix for IWP
  DATA_SCALAR(u1); // pc prior, u param for IWP 
  DATA_SCALAR(alpha1); // pc prior, alpha param for IWP
  
  DATA_SPARSE_MATRIX(B2); // Design matrix (for random effects 2: the one-year sGP)
  DATA_SPARSE_MATRIX(X2); // Design matrix (for boundary effects of the sGP)
  DATA_SPARSE_MATRIX(P2); // Penalty matrix for sGP
  DATA_SCALAR(logP2det); // Determinant of (fixed) penalty matrix for sGP
  DATA_SCALAR(u2); // pc prior, u param for sGP 
  DATA_SCALAR(alpha2); // pc prior, alpha param for sGP

  DATA_SCALAR(u_over); // pc prior, u param
  DATA_SCALAR(alpha_over); // pc prior, alpha param
  DATA_SCALAR(betaprec); // precision for beta

  // Parameter
  PARAMETER_VECTOR(W); 
  int d1 = P1.cols(); // Number of random effect elements in IWP
  int d2 = P2.cols(); // Number of random effect elements in sGP

  vector<Type> U1(d1);
  vector<Type> U2(d2);

  int beta1dim = X1.cols();
  int beta2dim = X2.cols();

  vector<Type> beta1(beta1dim);
  vector<Type> beta2(beta2dim);


  int n = R.cols(); // Number of (refined) data points (weeks)
  vector<Type> O(n);
  for (int i=0;i<d1;i++) U1(i) = W(i);
  for (int i=0;i<d2;i++) U2(i) = W(d1 + i);
  for (int i=0;i<beta1dim;i++) beta1(i) = W(i + d1 + d2);
  for (int i=0;i<beta2dim;i++) beta2(i) = W(i + d1 + d2 + beta1dim);
  for (int i=0;i<n;i++) O(i) = W(i + d1 + d2 + beta1dim + beta2dim);

  PARAMETER(theta1); // theta = -2log(sigma)
  PARAMETER(theta2); // theta = -2log(sigma)
  PARAMETER(theta_over); // theta = -2log(sigma)

  // Transformations
  vector<Type> eta =  B1 * U1 + X1 * beta1 + B2 * U2 + X2 * beta2 + O;

  // Log likelihood
  Type ll = 0;
  ll = sum(dpois(y, R * exp(eta), TRUE));
  
  // Log prior on W
  Type lpW = 0;

  // Cross product
  vector<Type> P1U1 = P1*U1;
  Type U1P1U1 = (U1 * P1U1).sum();
  lpW += -0.5 * exp(theta1) * U1P1U1; // U part
  Type bb1 = (beta1 * beta1).sum();
  lpW += -0.5 * betaprec * bb1; // Beta part

  vector<Type> P2U2 = P2*U2;
  Type U2P2U2 = (U2 * P2U2).sum();
  lpW += -0.5 * exp(theta2) * U2P2U2; // U part
  Type bb2 = (beta2 * beta2).sum();
  lpW += -0.5 * betaprec * bb2; // Beta part

  Type over_part = (O * O).sum();
  lpW += -0.5 * exp(theta_over) * over_part; // Beta part

  // Log determinant
  Type log1det = d1 * theta1 + logP1det;
  lpW += 0.5 * log1det; // P1 part
  Type log2det = d2 * theta2 + logP2det;
  lpW += 0.5 * log2det; // P2 part
  Type logdet_over = n * theta_over;
  lpW += 0.5 * logdet_over; // P part
  
  // Log prior for theta
  Type lpT = 0;
  Type phi1 = -log(alpha1) / u1;
  lpT += log(0.5 * phi1) - phi1*exp(-0.5*theta1) - 0.5*theta1;
  Type phi2 = -log(alpha2) / u2;
  lpT += log(0.5 * phi2) - phi2*exp(-0.5*theta2) - 0.5*theta2;

  Type phi_over = -log(alpha_over) / u_over;
  lpT += log(0.5 * phi_over) - phi_over*exp(-0.5*theta_over) - 0.5*theta_over;
  
  // Final result!
  Type logpost = -1 * (ll + lpW + lpT);
  
  return logpost;
}