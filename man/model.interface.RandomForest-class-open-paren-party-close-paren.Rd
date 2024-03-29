% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.interface.cforest.r
\name{model.interface.RandomForest-class (party)}
\alias{model.interface.RandomForest-class (party)}
\alias{model.interface.RandomForest.class}
\title{(Internal) model.interface class for cforest}
\description{
This reference class contains methods for \code{\link[party]{cforest}} in
\emph{party} package.
Note that because an object of RandomForest does not keep original call,
get.call() function always returns NULL.
}
\section{Super class}{
\code{\link[model.adapter:model.interface]{model.adapter::model.interface}} -> \code{model.interface.RandomForest}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-model.interface.RandomForest-get.call}{\code{model.interface.RandomForest.class$get.call()}}
\item \href{#method-model.interface.RandomForest-get.formula}{\code{model.interface.RandomForest.class$get.formula()}}
\item \href{#method-model.interface.RandomForest-get.data}{\code{model.interface.RandomForest.class$get.data()}}
\item \href{#method-model.interface.RandomForest-get.model.type}{\code{model.interface.RandomForest.class$get.model.type()}}
\item \href{#method-model.interface.RandomForest-predict}{\code{model.interface.RandomForest.class$predict()}}
\item \href{#method-model.interface.RandomForest-adjust.offset}{\code{model.interface.RandomForest.class$adjust.offset()}}
\item \href{#method-model.interface.RandomForest-clone}{\code{model.interface.RandomForest.class$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="expand.formula"><a href='../../model.adapter/html/model.interface.html#method-model.interface-expand.formula'><code>model.adapter::model.interface$expand.formula()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.family"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.family'><code>model.adapter::model.interface$get.family()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.link"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.link'><code>model.adapter::model.interface$get.link()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.linkinv"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.linkinv'><code>model.adapter::model.interface$get.linkinv()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="get.offset.names"><a href='../../model.adapter/html/model.interface.html#method-model.interface-get.offset.names'><code>model.adapter::model.interface$get.offset.names()</code></a></span></li>
<li><span class="pkg-link" data-pkg="model.adapter" data-topic="model.interface" data-id="initialize"><a href='../../model.adapter/html/model.interface.html#method-model.interface-initialize'><code>model.adapter::model.interface$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-get.call"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-get.call}{}}}
\subsection{Method \code{get.call()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$get.call(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-get.formula"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-get.formula}{}}}
\subsection{Method \code{get.formula()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$get.formula(x, envir, package = "")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-get.data"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-get.data}{}}}
\subsection{Method \code{get.data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$get.data(x, envir, package = "", ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-get.model.type"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-get.model.type}{}}}
\subsection{Method \code{get.model.type()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$get.model.type(x, envir, package = "", ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-predict"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-predict}{}}}
\subsection{Method \code{predict()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$predict(object, newdata = NULL, type, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-adjust.offset"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-adjust.offset}{}}}
\subsection{Method \code{adjust.offset()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$adjust.offset(x, envir, package, pred, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-model.interface.RandomForest-clone"></a>}}
\if{latex}{\out{\hypertarget{method-model.interface.RandomForest-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{model.interface.RandomForest.class$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
