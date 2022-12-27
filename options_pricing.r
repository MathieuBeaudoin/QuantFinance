### Modele d'arbre binomial ###

# Proba neutre au risque
Q <- function(escompte, u, d) (1/escompte - d) / (u - d)

# Fonction de paiement selon la valeur de l'actif, le strike price, et le type
# Pour les options exotiques, fournir la transformation des valeurs de l'actif
# applicable pour la variable S
pmt <- function(S, k, type) {
  if(type=="put") return(max(k-S, 0))
  else if(type=="call") return(max(S-k, 0))
}

# Valeur de l'action au temps t, noeud j, j appartient a {0,1,...,t}
Stj <- function(S0, u, d, j_max, j) S0*u**j*d**(j_max-j)

# Valeur de continuation au temps t
VCt <- function(Cu, Cd, q, escompte) escompte*(q*Cu + (1-q)*Cd)

# Put/call americain (pmt_prem=T) ou europeen (pmt_prem=F)
prix_vanille <- function(n_periodes, u, d, S0, k, escompte, type, pmt_prem=F) {
  Ctj = replicate(n_periodes, list())
  q = Q(escompte, u, d)
  Ctj[[n_periodes]] = lapply(0:n_periodes, function(j) pmt(Stj(S0,u,d,n_periodes,j), k, type))
  # Evaluation recursive
  for(t in (n_periodes-1):1) {
    Ctj[[t]] = rep(0,t+1)
    for(j in t:0) {
      VC = VCt(Ctj[[t+1]][[j+2]], Ctj[[t+1]][[j+1]], q, escompte)
      if(pmt_prem) Ctj[[t]][[j+1]] = max(VC, pmt(Stj(S0,u,d,t,j), k, type))
      else Ctj[[t]][[j+1]] = VC
    }
  }
  return(VCt(Ctj[[1]][2], Ctj[[1]][1], q, escompte))
}
prix_vanille(n_periodes=3, u=1.1, d=0.9, S0=100, k=100,
             escompte=exp(-0.05), type="put", pmt_prem=T)


# Generateur de chemins - pour options exotiques
gen_path <- function(cur_path, u, d, n_restant) {
  n = dim(cur_path)[1]
  j = dim(cur_path)[2]
  ups = cur_path[,j]*u
  downs = cur_path[,j]*d
  cur_path = cbind(rbind(cur_path, cur_path),
                   c(ups, downs))
  if(n_restant>1) cur_path = gen_path(cur_path, u, d, n_restant-1)
  return(cur_path)
}

# Options dont le paiement a l'echeance est base sur une fonction du chemin des prix (parametre FUN)
prix_exotique <- function(n_periodes, u, d, S0, k, escompte, type, FUN=mean, exclure=c(1)) {
  chemins = gen_path(as.matrix(S0), u, d, n_periodes)
  n = dim(chemins)[1]
  q = Q(escompte, u, d)
  print(paste("q =",q))
  S = ups = proba_NaR = pmt = rep(0,n)
  for(i in 1:n) {
    ups[i] = sum(chemins[i,2:(n_periodes+1)]>chemins[i,1:n_periodes])
    proba_NaR[i] = q**ups[i]*(1-q)**(n_periodes-ups[i])
    if(length(exclure)==0) S[i] = FUN(chemins[i,])
    else S[i] = FUN(chemins[i, -exclure])
    pmt[i] = pmt(S[i], k, type)
  }
  resume = data.frame(cbind(chemins, ups, S, pmt, proba_NaR))
  colnames(resume) = c(paste0("t",0:n_periodes), "u", "S", "paiement", "proba")
  print(resume[order(resume$paiement, decreasing=T), ])
  return(escompte**n_periodes*sum(proba_NaR * pmt))
}
prix_exotique(n_periodes=4, u=1.12, d=0.92, S0=50, k=55, escompte=exp(-0.05),
               type="call", FUN=max)

