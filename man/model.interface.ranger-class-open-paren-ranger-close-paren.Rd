% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.interface.ranger.r
\name{model.interface.ranger-class (ranger)}
\alias{model.interface.ranger-class (ranger)}
\alias{model.interface.ranger.class}
\title{(Internal) model.interface class for ranger}
\description{
This reference class contains methods for \code{\link[ranger]{ranger}} in
\emph{ranger} package.
}
\section{Super class}{
\code{\link[model.adapter:model.interface]{model.adapter::model.interface}} -> \code{model.interface.ranger}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-model.interface.ranger-predict}{\code{model.interface.ranger.class$predict()}}
\item \href{#method-model.interface.ranger-get.formula}{\code{model.interface.ranger.class$get.formula()}}
\item \href{#method-model.interface.ranger-clone}{\code{model.interface.ranger.class$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="adjust.offset"><a href='../../model.adapter/html/model.interface.html#method-model.interface-adjust.offset'><code>model.adapter::model.interface$adjust.offset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="expand.formula"><a href='../../model.adapter/html/model.interface.html#method-model.interface-expand.formula'><code>model.adapter::model.interface$expand.formula()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.call"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.call'><code>model.adapter::model.interface$get.call()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.data"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.data'><code>model.adapter::model.interface$get.data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.family"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.family'><code>model.adapter::model.interface$get.family()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.link"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.link'><code>model.adapter::model.interface$get.link()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.linkinv"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.linkinv'><code>model.adapter::model.interface$get.linkinv()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.model.type"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.model.type'><code>model.adapter::model.interface$get.model.type()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.offset.names"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.offset.names'><code>model.adapter::model.interface$get.offset.names()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="initialize"><a href='../../model.adapter/html/model.interface.html#method-model.interface-initialize'><code>model.adapter::model.interface$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.ranger-predict"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.ranger-predict}{}}}
\subsection{Method \code{predict()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.ranger.class$predict(object, newdata = NULL, type, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.ranger-get.formula"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.ranger-get.formula}{}}}
\subsection{Method \code{get.formula()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.ranger.class$get.formula(x, envir, package = "")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.ranger-clone"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.ranger-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.ranger.class$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
