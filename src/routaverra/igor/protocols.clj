(ns routaverra.igor.protocols)

(defprotocol IExpress
  (write [self])
  (domainv [self])
  (codomain [self])
  (decisions [self])
  (translate [self])
  (validate [self])
  (bindings [self])
  (evaluate [self solution]))

(defprotocol IExpand
  (expand [self]))

(defprotocol IInclude
  (mzn-includes [self]))
