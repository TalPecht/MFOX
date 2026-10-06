library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

source("R/data_load.R")
source("R/filters.R")
source("R/validation.R")
source("R/curation.R")

mfox <- load_mfox_data()
validation_errors <- validate_mfox(mfox)

theme <- bs_theme(version=5, bg="#FFFDFC", fg="#35141D", primary="#A11F46", secondary="#D86B72")

sample_label <- function(x) dplyr::recode(x,
  "Direct menstrual fluid"="Whole menstrual fluid",
  "Whole menstrual fluid"="Whole menstrual fluid",
  "Direct cellular fraction"="Menstrual-fluid cells",
  "Menstrual-fluid cells"="Menstrual-fluid cells",
  "Direct tissue fraction"="Tissue fragments",
  "Tissue fragments"="Tissue fragments",
  "Direct cellular/tissue fraction"="Mixed cells/tissue fraction — source did not separate",
  "Direct cells/tissue"="Mixed cells/tissue fraction — source did not separate",
  "Cells/tissue — not further separated"="Mixed cells/tissue fraction — source did not separate",
  "Cell-free fraction"="Cell-free menstrual fluid",
  "Cell-free menstrual fluid"="Cell-free menstrual fluid",
  "Extracellular vesicles"="Extracellular vesicles (EVs)",
  "Cultured menstrual-derived cells"="Cultured menstrual-derived cells",
  "Experimental derivatives"="Experimentally manipulated derivatives",
  .default=x)

mfox_palette_base <- c("#7A1832","#B83254","#E98567","#D69B54","#2F7E83","#5A668F","#8D5A8B","#A65C3A","#6B7D3D","#8797A5")
mfox_category_palette <- function(values){
  vals <- sort(unique(as.character(values[!is.na(values) & as.character(values)!=""])))
  setNames(rep(mfox_palette_base,length.out=length(vals)),vals)
}

link_or_text <- function(label,url) ifelse(!is.na(url)&url!="",paste0('<a href="',url,'" target="_blank">',label,' ↗</a>'),label)

workspace_panel <- function(id,title,...,class=NULL){
  div(id=id,class=paste("workspace-panel",class),
    div(class="workspace-head",h3(title),div(class="workspace-actions",
      tags$button(type="button",class="panel-btn panel-collapse",title="Collapse / expand","−"),
      tags$button(type="button",class="panel-btn panel-expand",title="Expand panel","⛶"),
      tags$button(type="button",class="panel-btn panel-close",title="Remove panel","×")
    )),
    div(class="workspace-body",...)
  )
}

workspace_js <- tags$script(HTML("
(function(){
  function closest(el, selector){ return el && el.closest ? el.closest(selector) : null; }
  function refreshRestore(){
    document.querySelectorAll('.restore-panel').forEach(function(btn){
      var target=document.getElementById(btn.dataset.target);
      btn.classList.toggle('d-none', !(target && target.classList.contains('is-removed')));
    });
  }
  function triggerResize(delay){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, delay || 100); }
  function markMfoxPageContent(){
    document.querySelectorAll('body > .container-fluid, body > .tab-content, body > main, main.bslib-page-main').forEach(function(el){
      if(!el.classList.contains('navbar') && !closest(el,'.navbar')) el.classList.add('mfox-page-content');
    });
  }
  document.addEventListener('click',function(e){
    var btn=closest(e.target,'.panel-collapse');
    if(btn){ var p=closest(btn,'.workspace-panel'); if(p) p.classList.toggle('is-collapsed'); return; }
    btn=closest(e.target,'.panel-expand');
    if(btn){ var p=closest(btn,'.workspace-panel'); if(p) p.classList.toggle('is-expanded'); triggerResize(150); return; }
    btn=closest(e.target,'.panel-close');
    if(btn){ var p=closest(btn,'.workspace-panel'); if(p) p.classList.add('is-removed'); refreshRestore(); return; }
    btn=closest(e.target,'.restore-panel');
    if(btn){ var t=document.getElementById(btn.dataset.target); if(t){t.classList.remove('is-removed','is-collapsed'); refreshRestore(); triggerResize(100);} return; }
    btn=closest(e.target,'.reset-workspace');
    if(btn){ document.querySelectorAll('.workspace-panel').forEach(function(p){p.classList.remove('is-removed','is-collapsed','is-expanded');}); refreshRestore(); triggerResize(100); return; }
    btn=closest(e.target,'.tech-home-link');
    if(btn && window.Shiny){ Shiny.setInputValue('home_tech_click',btn.dataset.family,{priority:'event'}); return; }
    btn=closest(e.target,'.intel-link');
    if(btn){
      var target=document.getElementById(btn.dataset.target);
      if(target){
        target.classList.remove('is-removed','is-collapsed'); refreshRestore();
        window.scrollTo({top:Math.max(0,target.getBoundingClientRect().top + window.scrollY - 90),behavior:'smooth'});
        target.classList.add('panel-highlight'); setTimeout(function(){target.classList.remove('panel-highlight');},1000); triggerResize(120);
      }
    }
  });
  document.addEventListener('DOMContentLoaded',function(){markMfoxPageContent();refreshRestore();});
  document.addEventListener('shiny:connected',function(){markMfoxPageContent();refreshRestore();setTimeout(markMfoxPageContent,80);});
})();
"))

# Voting is intentionally kept outside Shiny's input binding / reactive UI cycle.
# Plain HTML buttons send one event; a custom message updates only the clicked card.
vote_js <- tags$script(HTML("\n(function(){
  function voteBlock(key){ return document.getElementById('candidate-vote-block-' + key); }
  function setText(root, role, value){
    if(!root) return;
    var el=root.querySelector('[data-vote-role=\"' + role + '\"]');
    if(el) el.textContent=String(value);
  }
  function setPending(block, pending){
    if(!block) return;
    block.dataset.pending=pending ? '1' : '0';
    block.querySelectorAll('.candidate-vote-js').forEach(function(btn){
      btn.disabled=!!pending;
      btn.setAttribute('aria-busy',pending ? 'true' : 'false');
    });
  }
  function setInlineMessage(block, text, kind){
    if(!block) return;
    var el=block.querySelector('[data-vote-role=\"message\"]');
    if(!el) return;
    el.textContent=text || '';
    el.className='candidate-vote-message' + (kind ? ' is-' + kind : '');
  }
  function updateActive(block, direction){
    if(!block) return;
    block.querySelectorAll('.candidate-vote-js').forEach(function(btn){
      var active=btn.dataset.voteDirection===direction;
      btn.classList.toggle('is-active',active);
      btn.setAttribute('aria-pressed',active ? 'true' : 'false');
    });
  }
  function updateSummary(msg){
    var map={
      'vote-summary-voters':msg.voters,
      'vote-summary-for':msg.total_for,
      'vote-summary-against':msg.total_against,
      'vote-summary-net':msg.total_net
    };
    Object.keys(map).forEach(function(id){
      var el=document.getElementById(id);
      if(el && map[id]!==undefined && map[id]!==null) el.textContent=String(map[id]);
    });
  }
  document.addEventListener('click',function(e){
    var btn=e.target.closest ? e.target.closest('.candidate-vote-js') : null;
    if(!btn) return;
    e.preventDefault();
    e.stopPropagation();
    if(e.stopImmediatePropagation) e.stopImmediatePropagation();
    var block=voteBlock(btn.dataset.candidateKey);
    if(!block || block.dataset.pending==='1') return false;
    if(!window.Shiny || !Shiny.setInputValue){
      setInlineMessage(block,'Voting is not connected. Reload the app and try again.','error');
      return false;
    }
    registerVoteAck();
    setPending(block,true);
    setInlineMessage(block,'Saving vote…','pending');
    var nonce=Date.now().toString() + '-' + Math.random().toString(36).slice(2);
    Shiny.setInputValue('candidate_vote_event',{
      candidate_id:btn.dataset.candidateId,
      candidate_key:btn.dataset.candidateKey,
      direction:btn.dataset.voteDirection,
      nonce:nonce
    },{priority:'event'});
    window.setTimeout(function(){
      if(block && block.dataset.pending==='1'){
        setPending(block,false);
        setInlineMessage(block,'No response from the app. Please try again.','error');
      }
    },8000);
    return false;
  },true);

  var voteAckRegistered=false;
  function registerVoteAck(){
    if(voteAckRegistered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler('candidate_vote_ack',function(msg){
      var block=voteBlock(msg.candidate_key);
      if(!block) return;
      setPending(block,false);
      if(!msg.ok){
        setInlineMessage(block,msg.message || 'Vote could not be recorded.','error');
        return;
      }
      setText(block,'balance',(msg.balance>0 ? '+' : '') + msg.balance);
      setText(block,'for',msg.votes_for);
      setText(block,'against',msg.votes_against);
      updateActive(block,msg.direction);
      updateSummary(msg);
      var note=msg.persisted ? 'Vote recorded.' : 'Vote recorded for this session only; this deployment cannot save votes to disk.';
      setInlineMessage(block,note,msg.persisted ? 'success' : 'warning');
    });
    voteAckRegistered=true;
  }
  document.addEventListener('shiny:connected',registerVoteAck);
  document.addEventListener('DOMContentLoaded',registerVoteAck);
  registerVoteAck();
})();\n"))



split_company_terms <- function(x){
  z <- trimws(unlist(str_split(as.character(x[!is.na(x) & as.character(x)!=""]),";")))
  sort(unique(z[nzchar(z)]))
}

candidate_vote_key <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
company_term_match <- function(x, selected){
  if(!length(selected)) return(rep(TRUE,length(x)))
  vapply(as.character(x),function(v){
    if(is.na(v) || !nzchar(v)) return(FALSE)
    any(trimws(str_split(v,";",simplify=FALSE)[[1]]) %in% selected)
  },logical(1))
}

company_text_has_any <- function(text, terms){
  terms <- unique(tolower(trimws(as.character(terms))))
  terms <- terms[nzchar(terms)]
  if(!length(terms)) return(rep(FALSE,length(text)))
  vapply(tolower(coalesce(as.character(text),"")),function(z){
    any(vapply(terms,function(term) grepl(term,z,fixed=TRUE),logical(1)))
  },logical(1))
}

plan_condition_terms <- function(x){
  x <- tolower(as.character(x %||% ""))
  if(!nzchar(x) || x=="any") return(character())
  out <- x
  dictionary <- list(
    endometriosis=c("endometriosis"),
    pcos=c("pcos","pmod"),
    infertility=c("infertility","fertility"),
    `recurrent pregnancy loss`=c("recurrent pregnancy loss","pregnancy loss"),
    adenomyosis=c("adenomyosis"),
    fibroid=c("fibroid","fibroids"),
    hpv=c("hpv","cervical"),
    autoimmune=c("autoimmune"),
    dysmenorrhea=c("dysmenorrhea","menstrual pain")
  )
  for(key in names(dictionary)) if(grepl(key,x,fixed=TRUE)) out <- c(out,dictionary[[key]])
  unique(out)
}

plan_condition_overlap_label <- function(x){
  x <- as.character(x %||% "")
  if(grepl("infertility",x,ignore.case=TRUE)) return("Fertility / infertility")
  if(grepl("dysmenorrhea",x,ignore.case=TRUE)) return("Dysmenorrhea / menstrual pain")
  x
}

plan_technology_family_label <- function(x){
  x <- as.character(x %||% "")
  switch(x,
    "Flow cytometry"="Flow / cellular phenotyping",
    "16S rRNA sequencing"="Microbiome sequencing",
    "Bulk transcriptomics"="RNA / transcriptomics",
    "Single-cell transcriptomics"="RNA / transcriptomics",
    "Single-nucleus transcriptomics"="RNA / transcriptomics",
    "Genomics"="DNA / genomics",
    "Epigenomics/methylation"="Epigenomics / methylation",
    "Proteomics"="Protein / proteomics",
    "Metabolomics"="Metabolite / metabolomics",
    "Lipidomics"="Lipid / lipidomics",
    "Metatranscriptomics"="Microbial RNA / metatranscriptomics",
    x
  )
}

plan_technology_terms <- function(x){
  x <- as.character(x %||% "")
  if(!nzchar(x) || x=="Any") return(character())
  switch(x,
    "Flow cytometry"=c("flow cytometry","cytometry","cell phenotyping"),
    "16S rRNA sequencing"=c("16s","microbiome","microbial","microbial sequencing"),
    "Bulk transcriptomics"=c("rna-seq","rna seq","transcript","rna expression","gene expression"),
    "Single-cell transcriptomics"=c("single-cell","single cell","rna-seq","rna seq","transcript","rna expression"),
    "Single-nucleus transcriptomics"=c("single-nucleus","single nucleus","rna-seq","rna seq","transcript","rna expression"),
    "Genomics"=c("genomic","genomics","dna","sequencing","pcr","qpcr"),
    "Epigenomics/methylation"=c("epigen","methyl","dna methyl"),
    "Proteomics"=c("proteom","protein biomarker","protein biomarkers"),
    "Metabolomics"=c("metabolom","metabolite","metabolites"),
    "Lipidomics"=c("lipidom","lipid","lipids"),
    "Metatranscriptomics"=c("metatranscript","microbiome","microbial","rna-seq","rna seq"),
    tolower(x)
  )
}

plan_sample_terms <- function(x){
  x <- as.character(x %||% "")
  if(!nzchar(x) || x=="Any") return(character())
  switch(x,
    "Direct menstrual fluid"=c("whole menstrual fluid","menstrual fluid","menstrual blood"),
    "Direct cellular fraction"=c("menstrual-fluid cells","menstrual fluid cells","cellular fraction"),
    "Direct tissue fraction"=c("tissue fragments","tissue fraction"),
    "Direct cellular/tissue fraction"=c("menstrual-fluid cells","tissue fragments","cells/tissue"),
    "Cell-free fraction"=c("cell-free","proteins / hormones","metabolites / lipids"),
    "Extracellular vesicles"=c("extracellular vesicles","evs"),
    "Cultured menstrual-derived cells"=c("cultured menstrual-derived cells","cultured","mesenchymal stem cells"),
    "Cultured-cell derivatives"=c("extracellular vesicles","conditioned medium","cell-derived products"),
    "Experimental derivatives"=c("experimental derivatives","cultured menstrual-derived cells"),
    tolower(sample_label(x))
  )
}

company_plan_overlap <- function(companies,q){
  d <- companies
  if(!nrow(d)) return(d)
  condition_text <- paste(d$disease_application,d$utilization_domain,d$technology,sep=" | ")
  technology_text <- paste(d$technology,d$utilization_domain,d$evidence_note,sep=" | ")
  sample_text <- paste(d$sample_type,d$material_exploited,d$sample_collection,sep=" | ")
  d$match_condition <- if(q$condition!="Any") company_text_has_any(condition_text,plan_condition_terms(q$condition)) else FALSE
  d$match_technology <- if(q$omics!="Any") company_text_has_any(technology_text,plan_technology_terms(q$omics)) else FALSE
  d$match_sample <- if(q$biospecimen!="Any") company_text_has_any(sample_text,plan_sample_terms(q$biospecimen)) else FALSE
  d$match_longitudinal <- if(q$longitudinal=="Yes") company_text_has_any(d$utilization_domain,c("longitudinal monitoring")) else FALSE
  d$overlap_count <- as.integer(d$match_condition)+as.integer(d$match_technology)+as.integer(d$match_sample)+as.integer(d$match_longitudinal)
  primary_requested <- q$condition!="Any" || q$omics!="Any"
  d$primary_overlap <- if(primary_requested) d$match_condition | d$match_technology else d$match_sample | d$match_longitudinal
  d |> filter(primary_overlap) |> arrange(desc(overlap_count),company)
}

entity_initials <- function(x){
  y <- trimws(gsub("[^[:alnum:] ]+"," ",as.character(x %||% "")))
  words <- unlist(strsplit(y,"[[:space:]]+"))
  words <- words[nzchar(words)]
  if(!length(words)) return("•")
  if(length(words)==1) return(toupper(substr(words[1],1,2)))
  paste0(toupper(substr(words[1],1,1)),toupper(substr(words[2],1,1)))
}
entity_mark <- function(label, icon_name=NULL){
  div(class="entity-mark",if(!is.null(icon_name)) icon(icon_name) else span(entity_initials(label)))
}

tab_overview <- function(icon_name,title,text,note=NULL){
  div(class="tab-overview-strip",
    div(class="tab-overview-icon",icon(icon_name)),
    div(class="tab-overview-copy",span(class="tab-overview-kicker","IN THIS VIEW"),h4(title),p(text),if(!is.null(note)) tags$small(note))
  )
}

section_overview <- function(purpose,inside,start){
  div(class="section-overview",
    div(class="section-overview-item",div(class="section-overview-icon",icon("bullseye")),div(tags$small("PURPOSE"),strong(purpose))),
    div(class="section-overview-item",div(class="section-overview-icon",icon("layer-group")),div(tags$small("INSIDE"),strong(inside))),
    div(class="section-overview-item",div(class="section-overview-icon",icon("arrow-pointer")),div(tags$small("START HERE"),strong(start)))
  )
}

ui <- page_navbar(
  id="main_nav",
  title=div(class="brand-wrap",tags$img(src="mfox_logo.png",height="56px"),div(tags$strong("MFOX"),tags$small("-Menstrual Fluid Omics eXplorer"))),
  theme=theme,
  fillable=FALSE,
  header=tagList(tags$link(rel="stylesheet",type="text/css",href="mfox.css"),workspace_js,vote_js),

  nav_panel("Home",
    div(class="home-shell",
      div(class="home-hero-compact",
        tags$img(src="mfox_logo.png",class="home-logo"),
        div(class="home-copy",div(class="eyebrow","A living map of menstrual-fluid research"),h1("Menstrual fluid, mapped from sample to science"),p("A living knowledgebase connecting what is collected, how it is measured, the papers and datasets it generates, and the projects, labs and companies shaping the field."))
      ),
      div(class="home-three",
        div(class="home-panel sample-mini",h3(icon("droplet")," The sample"),p("One collected specimen can lead to distinct biological materials."),
          div(class="sample-tree",span(class="root-chip","Menstrual fluid"),div(class="mini-arrow","↓"),div(class="sample-chips",span("Whole fluid"),span("Cells"),span("Tissue"),span("Cell-free"),span("EVs"),span("Cultured cells"))),
          tags$details(tags$summary("How MFOX defines samples"),p("Whole fluid is unfractionated; cells and tissue are recovered directly; cell-free is the soluble/supernatant fraction; EVs are isolated vesicles; cultured cells have undergone ex-vivo expansion."))
        ),
        div(class="home-panel",h3(icon("microscope")," Measurement layers"),p("Assay technology is kept separate from sample type and biological domain."),uiOutput("home_tech_layers")),
        div(class="home-panel",h3(icon("chart-simple")," Field snapshot"),uiOutput("home_metrics"))
      ),
      div(class="home-nav",
        actionButton("home_landscape","Explore evidence",icon=icon("table-cells-large"),class="home-action"),
        actionButton("home_community","Find researchers",icon=icon("people-group"),class="home-action"),
        actionButton("home_plan","Plan a study",icon=icon("compass-drafting"),class="home-action"),
        actionButton("home_data","Find data",icon=icon("database"),class="home-action"),
        actionButton("home_companies","Explore utilization",icon=icon("briefcase-medical"),class="home-action")
      ),
      div(class="home-join",div(h4("Help make MFOX more complete"),p("Submit a missing paper, dataset, company, researcher, correction or other menstrual-fluid resource—even when the publication uses different terminology.")),actionButton("home_contribute","Contribute to MFOX →",class="btn-outline-primary"))
    )
  ),

  nav_panel("Landscape",
    div(class="page-intro compact-intro data-page-intro",h2("Evidence landscape"),p("Use Landscape for high-level patterns: see where evidence is concentrated, sparse or changing over time. Use Explore when you want the individual studies, papers, datasets and organizations behind those patterns.")),
    section_overview(
      "See field-level patterns and evidence gaps.",
      "Matrix, bubbles, composition and time trends across samples, technologies and contexts.",
      "Choose Compare, By and Measure; then switch visualization without changing the evidence scope."
    ),
    div(class="workspace-toolbar",
      actionButton("reset_landscape","Reset layout",class="btn-sm btn-outline-secondary reset-workspace"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="land_filters","+ Filters"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="land_visual","+ Visualization"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="land_details","+ Evidence details")
    ),
    div(class="landscape-workspace",
      workspace_panel("land_filters","Filters",class="filter-panel",
        selectInput("scope","Evidence scope",c("All menstrual-derived evidence","Direct menstrual fluid only","Cultured menstrual-derived cells","Experimental derivatives")),
        selectInput("landscape_row","Compare",c("Clinical context"="clinical_context","Sample type"="biospecimen_class","Research domain"="research_domain","Assay family"="assay_family")),
        selectInput("landscape_col","By",c("Technology"="omics_modality","Assay family"="assay_family","Sample type"="biospecimen_class","Research domain"="research_domain","Clinical condition"="condition")),
        selectInput("landscape_measure","Measure",c("Studies"="studies","Assays"="assays")),
        selectInput("landscape_view","View",c("Evidence matrix"="heatmap","Bubble matrix"="bubble","Composition"="bars","Timeline"="timeline")),
        checkboxInput("landscape_transpose","Transpose axes",FALSE),
        selectInput("landscape_sort","Order categories",c("Evidence density"="density","Alphabetical"="alpha")),
        p(class="sidebar-note","Tip: expand the visualization panel for large matrices. Hover for full labels and counts.")),
      workspace_panel("land_visual","Evidence visualization",class="visual-panel",uiOutput("landscape_kpis"),uiOutput("landscape_view_guide"),plotlyOutput("landscape_plot",height="820px")),
      workspace_panel("land_details","Evidence details",class="details-panel",DTOutput("landscape_studies"))
    )
  ),

  nav_panel("Explore",
    div(class="page-intro compact-intro data-page-intro explore-knowledge-intro",
      div(class="eyebrow","DISCOVERY MODE · CONNECTED KNOWLEDGEBASE"),
      h2("Explore what already exists"),
      p("Filter once, then move across curated studies/assays, public study-registry records, relationships, papers, datasets, projects and organizations.")
    ),
    section_overview(
      "Trace the individual evidence behind the field.",
      "Connections · Studies & assays · Registered studies · Papers · Datasets · Projects/programs · Labs & companies.",
      "Leave filters at Any to browse everything, or narrow context, technology and sample once for all views."
    ),
    div(class="explore-filter-guide",
      div(class="explore-filter-guide-head",
        div(class="eyebrow","CHOOSE YOUR EVIDENCE LENS"),
        h3("Filter the same scientific space across every Explore view"),
        p("Start with any one dimension below, combine them when useful, or leave all three at Any. Curated evidence follows exact metadata; registry records follow when the registry reports the relevant field.")),
      div(class="explore-filter-guide-steps",
        div(strong("1 · Clinical context"),span("Disease, physiological setting or research question.")),
        div(strong("2 · Technology"),span("How the menstrual-fluid sample was measured.")),
        div(strong("3 · Sample type"),span("The menstrual-fluid material that entered the assay."))
      )
    ),
    div(class="data-filterbar explore-data-filterbar",
      selectizeInput("explore_population","1 · Clinical context",choices=NULL,multiple=TRUE,options=list(placeholder="Any · all clinical contexts")),
      selectizeInput("explore_omics","2 · Technology",choices=NULL,multiple=TRUE,options=list(placeholder="Any · all technologies")),
      selectizeInput("explore_biospecimen","3 · Sample type",choices=NULL,multiple=TRUE,options=list(placeholder="Any · all sample types")),
      div(class="filterbar-actions",
        checkboxInput("public_only","Public dataset only",FALSE),
        actionButton("clear_filters","Reset to Any",class="btn-sm btn-outline-secondary")
      ),
      div(class="filterbar-help-row",
        div(class="filterbar-note",icon("filter"),span("Any leaves a dimension open. Specific values narrow the same connected evidence across every Explore tab.")),
        tags$details(class="sample-definition-help compact-help",
          tags$summary(icon("circle-info")," Sample-type rules"),
          p(strong("Sample type is curated from source material and processing, not from assay name alone.")),
          p(strong("Mixed cells/tissue fraction — source did not separate")," is used when the source does not let MFOX distinguish a purified cell suspension from tissue fragments confidently."),
          p(class="muted small","Methods, processing steps and repository metadata take precedence over assay modality."))
      )
    ),
    div(class="data-canvas explore-data-canvas",
      navset_card_tab(
        id="explore_tabs",
        nav_panel("Connections",
          div(class="explore-main-stack",
            tab_overview("diagram-project","Connections","See how the filtered evidence is linked. Evidence flow connects samples and technologies to studies and downstream resources; scientific relationships show co-occurrence between technologies, samples, clinical questions and research domains.","Connection width/count reflects representation in MFOX, not evidence quality or importance."),
            selectInput("explore_connection_mode","Connection view",c("Evidence flow"="flow","Scientific relationships"="science")),
            conditionalPanel("input.explore_connection_mode == 'flow'",
              card(class="explore-connection-card",card_header("Evidence flow"),
                div(class="visual-legend-note",icon("circle-nodes"),span("Sample type → technology → resource. Resources include papers, reusable datasets, projects/programs, institutions and companies; width reflects represented links.")),
                plotlyOutput("explore_connection_map",height="620px"))
            ),
            conditionalPanel("input.explore_connection_mode == 'science'",
              card(class="explore-connection-card",card_header("Scientific relationships"),
                layout_columns(col_widths=c(5,7),
                  selectInput("explore_network_view","Connect",c("Technologies ↔ diseases/questions"="technology_disease","Samples ↔ technologies"="sample_technology","Technologies ↔ research domains"="technology_domain")),
                  div(class="view-guide",icon("filter"),span("This network follows the main Explore filters above; no second filter layer is needed."))
                ),
                plotlyOutput("explore_scientific_network",height="660px"),
                p(class="muted small","Connection width reflects the number of represented assay-level records. Categories can overlap; this is descriptive, not a ranking."))
            )
          )
        ),
        nav_panel("Studies & assays",
          div(class="explore-main-stack",
            tab_overview("circle-nodes","Evidence overview","Start here for the scientific core: see how samples and technologies connect to curated studies, then inspect the underlying assay-level records.","Counts and links follow the filters above; they describe representation, not evidence quality."),
            uiOutput("explore_graph_metrics"),
            card(card_header("Curated studies and assays"),
              uiOutput("explore_evidence_guidance"),
              div(class="explore-evidence-note",icon("droplet"),span("Sample labels describe the input material. Assay modality describes how that material was measured; the two are curated separately.")),
              DTOutput("explore_table"))
          )
        ),
        nav_panel("Registered studies",
          div(class="explore-main-stack",
            tab_overview("clipboard-list","Registered studies","Find menstrual-fluid studies registered across public clinical-study registries, including studies that may be ongoing or not yet published. Registry records are kept separate from publication-derived assay evidence.","A registry entry documents a planned or ongoing study; it does not mean results are available or independently validated."),
            div(class="registry-source-filter",
              selectInput("registry_source_filter","Registry source",
                choices=c("All registries"="__ALL__",sort(unique(mfox$registry_studies$registry))),
                selected="__ALL__")
            ),
            uiOutput("registry_study_metrics"),
            tags$details(class="registry-coverage-details",
              tags$summary(icon("earth-europe")," Registry search coverage"),
              p(class="muted","MFOX keeps the original registry as the canonical source and uses WHO ICTRP as a cross-registry discovery layer. A source can be in the search scope even when no menstrual-fluid record has yet passed manual verification."),
              DTOutput("registry_sources_table")
            ),
            card(card_header("Public study-registry records"),
              div(class="registry-note",icon("circle-info"),span("MFOX searches registry text and linked protocols for menstrual effluent, menstrual blood, menstrual fluid, menstruum and menstrual-blood-derived cells, then manually verifies that the specimen or derived product is actually collected, analyzed or used. Curated records now include ClinicalTrials.gov, ISRCTN, DRKS, the Netherlands registry, ChiCTR and ReBEC; additional WHO-primary registries are included in the active search scope. ‘Public dataset only’ keeps records linked to curated MFOX evidence with an indexed dataset.")),
              DTOutput("explore_registry_table"))
          )
        ),
        nav_panel("Papers",
          div(class="explore-main-stack",
            tab_overview("file-lines","Papers","Find the publications behind the filtered evidence and follow their linked study, dataset and project/program relationships."),
            card(card_header("Papers"),
              DTOutput("explore_papers_table"))
          )
        ),
        nav_panel("Datasets",
          div(class="explore-main-stack",
            tab_overview("database","Reusable datasets","Find public accessions generated by the filtered studies. MFOX links each accession back to its assay, sample and paper; it does not host the raw files."),
            card(card_header("Reusable datasets"),
              DTOutput("explore_datasets_table"))
          )
        ),
        nav_panel("Projects & programs",
          div(class="explore-main-stack",
            tab_overview("diagram-project","Projects & programs","See grants, consortia and research programs explicitly linked to the filtered evidence. Missing links are left blank rather than inferred."),
            card(card_header("Research projects, consortia & programs"),
              DTOutput("explore_projects_table"))
          )
        ),
        nav_panel("Labs & companies",
          div(class="explore-main-stack explore-entity-stack",
            tab_overview("building-columns","Labs & companies","See the people/organizations connected to this evidence subset. Academic institutions follow publication authorship; company links require curated relationships."),
            card(card_header("Publication-linked labs & institutions"),
              DTOutput("explore_labs_table")),
            card(card_header("Companies & translation"),
              DTOutput("explore_companies_table"))
          )
        )
      )
    )
  ),


  nav_panel("Companies",
    div(class="page-intro compact-intro data-page-intro companies-discovery-intro",
      div(class="eyebrow","TRANSLATION & UTILIZATION"),
      h2("What can menstrual fluid be used for?"),
      p("Explore the commercial and translational landscape of menstrual-fluid utilization. Companies remain discoverable even when they are not yet linked to a curated MFOX paper or project.")
    ),
    section_overview(
      "Explore WHAT menstrual fluid is being used for and HOW organizations are translating it — not a validation ranking.",
      "Company directory with evidence + maturity · Material → company → utilization relationships.",
      "Use this section for the utilization landscape; Community → Companies is the WHO / WHERE view of organizations in the field."
    ),
    uiOutput("company_metrics"),
    div(class="data-filterbar company-data-filterbar",
      textInput("company_search","Search",placeholder="Company, technology or application"),
      selectizeInput("company_use","Utilization",choices=NULL,multiple=TRUE,options=list(placeholder="All utilization areas")),
      selectizeInput("company_material","Material / component",choices=NULL,multiple=TRUE,options=list(placeholder="All material classes")),
      selectizeInput("company_maturity","Development stage",choices=NULL,multiple=TRUE,options=list(placeholder="All stages")),
      selectizeInput("company_evidence","Evidence status",choices=NULL,multiple=TRUE,options=list(placeholder="All evidence types")),
      div(class="filterbar-actions company-filter-actions",
        actionButton("clear_company_filters","Clear filters",class="btn-sm btn-outline-secondary")
      ),
      div(class="filterbar-help-row",
        div(class="filterbar-note",icon("compass"),span("Discovery is intentionally broader than Explore: companies remain visible even without a linked MFOX publication."))
      )
    ),
    div(class="data-canvas company-data-canvas",
      navset_card_tab(
        id="company_tabs",
        nav_panel("Company directory",
          div(class="company-discovery-stack",
            tab_overview("building","Company directory","Browse every curated company in the current filters. Development stage and evidence status are shown here directly so you can interpret each organization without switching views.","Evidence status describes the strongest public evidence type currently curated by MFOX; it is not a score."),
            uiOutput("company_evidence_summary"),
            uiOutput("company_overview_cards"))
        ),
        nav_panel("Utilization explorer",
          div(class="company-discovery-stack",
            tab_overview("compass","Utilization explorer","Explore the translation space as a connected system: which menstrual-fluid materials feed which uses, and which organizations are working in each area.","Flow width reflects the number of curated company relationships, not market size or scientific validation."),
            card(class="company-utilization-flow-card",card_header("Material → company → utilization"),
              div(class="visual-legend-note",icon("diagram-project"),span("Follow menstrual-fluid material into the companies using it, then into the utilization areas those companies are pursuing.")),
              plotlyOutput("company_utilization_flow",height="680px")),
            card(card_header("Where is activity concentrated?"),
              p(class="muted small","Each cell counts companies that list both a material class and a utilization area; it does not imply a one-to-one product claim."),
              plotlyOutput("company_material_use_heatmap",height="520px")),
            card(card_header("Utilization areas"),
              p(class="muted small","A company can appear in more than one area. Cards summarize the current filtered company set."),
              uiOutput("company_utilization_map"))
          )
        )
      )
    )
  ),

  nav_panel("Community",
    div(class="page-intro compact-intro data-page-intro community-page-intro",
      h2("The menstrual-fluid research community"),
      p("Explore who is working in the field, which groups recur across publications, where expertise is located, and which organizations are active in translation.")),
    section_overview(
      "Find WHO is building the field and WHERE the expertise sits; use Companies for the separate WHAT / HOW utilization landscape.",
      "Collaboration groups · Researchers & labs · Companies & innovators · Geography · Community suggestions.",
      "Start with Collaboration groups for the field overview, then move into people, organizations or geography."
    ),
    div(class="community-overview community-overview-compact",
      div(class="community-overview-grid",
        actionButton("community_go_groups",label=tagList(icon("people-roof"),div(strong("Collaboration groups"),span("Start with the groups and institutions shaping the field."))),class="community-guide-card"),
        actionButton("community_go_people",label=tagList(icon("user-group"),div(strong("Researchers & labs"),span("Dive into publication-linked people and expertise."))),class="community-guide-card"),
        actionButton("community_go_companies",label=tagList(icon("building"),div(strong("Companies & innovators"),span("See who is active in translation; use Companies for utilization detail."))),class="community-guide-card"),
        actionButton("community_go_geo",label=tagList(icon("earth-europe"),div(strong("Geography"),span("Map research institutions and companies together."))),class="community-guide-card"),
        actionButton("community_go_join",label=tagList(icon("user-plus"),div(strong("Join the Community"),span("Submit a structured researcher, lab or company profile for curator review."))),class="community-guide-card")
      )
    ),
    navset_card_tab(id="community_tabs",
      nav_panel("Collaboration groups",
        tab_overview("people-roof","Collaboration groups","Start here for a welcoming overview of the research community: recurring institutions, group size, publication activity and representative work.","This is a publication-linked view of the research community; it does not rank groups by quality or influence."),
        uiOutput("community_summary_metrics"),
        uiOutput("collaboration_groups")),
      nav_panel("Researchers & labs",
        tab_overview("user-group","Researchers & labs","Dive into publication-linked researchers and institutions after orienting yourself in Collaboration groups. Filter by country or MFOX Community inclusion criteria."),
        layout_sidebar(sidebar=sidebar(textInput("people_search","Find a researcher or institution"),selectizeInput("people_country","Country",choices=NULL,multiple=TRUE),checkboxInput("people_visible_only","Show researchers meeting MFOX Community inclusion criteria",TRUE),tags$details(class="eligibility-help",tags$summary("Who is included?"),p("The main directory includes first, last or corresponding authors; explicitly equal-contributing authors; and researchers appearing on more than one eligible menstrual-fluid publication. Other captured authors remain in the underlying authorship database."))),DTOutput("people_table"))),
      nav_panel("Companies & innovators",
        tab_overview("building","Companies & innovators","Community view = WHO and WHERE: browse organizations active in menstrual-fluid translation. The main Companies section is the separate WHAT and HOW utilization explorer."),
        div(class="community-company-distinction",
          div(icon("users"),div(strong("Community view"),span("Who is active, where they are based, and their broad area of work."))),
          div(icon("compass"),div(strong("Companies view"),span("What menstrual fluid is used for, which materials are exploited, and the maturity/evidence landscape."))),
          actionButton("community_open_companies","Open utilization landscape",class="btn-sm btn-outline-primary")),
        layout_sidebar(
          sidebar=sidebar(
            textInput("community_company_search","Find a company, technology or application"),
            selectizeInput("community_company_country","Country",choices=NULL,multiple=TRUE),
            selectizeInput("community_company_use","Utilization area",choices=NULL,multiple=TRUE),
            p(class="sidebar-note","This directory intentionally stays lighter than the main Companies section to avoid duplicating the utilization explorer.")),
          uiOutput("community_company_cards")
        )),
      nav_panel("Geography",
        tab_overview("earth-europe","Geography","Map the field as one ecosystem: research institutions and companies are shown together with distinct marker colors. Filters apply to both where matching metadata is available."),
        layout_columns(col_widths=c(4,4,4),selectizeInput("map_technology","Technology",choices=NULL,multiple=TRUE),selectizeInput("map_sample","Sample type",choices=NULL,multiple=TRUE),selectizeInput("map_disease","Disease / question",choices=NULL,multiple=TRUE)),uiOutput("map_coverage"),leafletOutput("people_map",height="720px")),
      nav_panel("Join the Community",
        tab_overview("user-plus","Join the Community","Submit a structured researcher, lab or company profile for curator review. The fields mirror the public MFOX directories so accepted information can later populate the relevant tables without being re-entered."),
        div(class="join-panel",
          h3("Add yourself, your group or your organization"),
          p("You do not need to have published menstrual-fluid work yet. Choose the profile type and sector first; MFOX then captures the same structured information used in the Research Community and Companies directories."),
          layout_columns(col_widths=c(6,6),
            selectInput("join_profile_type","Profile type",c("Individual researcher","Research group / lab","Company / innovator","Other organization")),
            selectInput("join_sector","Sector",c("Academia / research institution","Company / industry","Clinical / healthcare","Non-profit / foundation","Other")),
            textInput("join_name","Name / lab / company"),
            textInput("join_institution","Institution / organization"),
            textInput("join_role","Role / title"),
            textInput("join_website","Website"),
            textInput("join_city","City"),
            textInput("join_region","Region / state"),
            textInput("join_country","Country"),
            textInput("join_profile","Institutional / public profile URL"),
            textInput("join_orcid","ORCID (if applicable)"),
            textInput("join_verification","Public project, grant, publication or other verification URL")
          ),
          h4("Menstrual-fluid activity"),
          layout_columns(col_widths=c(6,6),
            selectizeInput("join_context","Clinical context / questions",choices=nz_choices(mfox$evidence$clinical_context),multiple=TRUE,options=list(placeholder="Select all that apply")),
            selectizeInput("join_samples","Sample types",choices=setNames(nz_choices(mfox$evidence$biospecimen_class),sample_label(nz_choices(mfox$evidence$biospecimen_class))),multiple=TRUE,options=list(placeholder="Select all that apply")),
            selectizeInput("join_technologies","Technologies / assays",choices=nz_choices(mfox$evidence$omics_modality),multiple=TRUE,options=list(placeholder="Select all that apply")),
            textInput("join_expertise","Expertise tags",placeholder="e.g. immunology; single-cell; biomarkers"),
            div(class="join-wide",textAreaInput("join_interests","Current menstrual-fluid work / research interests",rows=3)),
            div(class="join-wide",textAreaInput("join_collaboration","Collaboration interests",rows=2,placeholder="Methods, cohorts, validation, data sharing, translation..."))
          ),
          conditionalPanel("input.join_profile_type == 'Company / innovator' || input.join_sector == 'Company / industry'",
            div(class="join-company-fields",
              h4("Company / translation details"),
              layout_columns(col_widths=c(6,6),
                selectizeInput("join_company_use","Utilization area(s)",choices=split_company_terms(mfox$companies$utilization_domain),multiple=TRUE,options=list(create=TRUE,plugins=list("remove_button"),placeholder="Select or add utilization areas")),
                selectizeInput("join_company_material","Material / component exploited",choices=split_company_terms(mfox$companies$material_exploited),multiple=TRUE,options=list(create=TRUE,plugins=list("remove_button"),placeholder="Select or add materials")),
                textInput("join_company_technology","Technology / product"),
                textInput("join_company_application","Disease / application"),
                selectInput("join_company_maturity","Development stage",c("Not sure / curator to assign",sort(unique(na.omit(mfox$companies$maturity_level))))),
                selectInput("join_company_evidence","Evidence status",c("Not sure / curator to assign",sort(unique(na.omit(mfox$companies$evidence_status)))))
              )
            )
          ),
          div(class="join-actions",
            actionButton("prepare_join","Review details",class="btn-outline-primary"),
            actionButton("submit_join","Submit for curator review",class="btn-primary")
          ),
          uiOutput("join_status"),
          p(class="muted small","Nothing is published automatically. Curators verify submissions before accepted fields are promoted into the Researchers/Labs or Companies tables.")
        ))
    )
  ),

  nav_panel("Plan a Study",
    div(class="page-intro compact-intro data-page-intro planner-purpose-intro",
      div(class="eyebrow","PLANNING MODE · DESIGN INTELLIGENCE"),
      h2("Plan a Study"),
      p("Define the study you are considering. MFOX returns targeted precedent for design, reusable datasets, experts and companies with relevant overlap; use Explore instead when you simply want to browse what exists.")
    ),
    section_overview(
      "Turn a study idea into evidence-backed design intelligence.",
      "Matching precedent · Design snapshot · Reusable datasets · Experts · Companies with overlap.",
      "Set at least one specific feature, then click Build study intelligence."
    ),
    div(class="planner-querybar",
      selectInput("plan_condition","Clinical context",choices=NULL),
      selectInput("plan_omics","Technology",choices=NULL),
      selectInput("plan_biospecimen","Sample type",choices=NULL),
      selectInput("plan_longitudinal","Design",c("Any","Yes","No")),
      actionButton("plan_run","Build study intelligence",class="btn-primary")
    ),
    p(class="planner-query-hint","You can leave any dimension as Any. Choose at least one specific feature so the planner can return targeted design precedent rather than simply reproducing Explore."),
    div(class="sample-help-inline",tags$details(tags$summary("What do the sample types mean?"),
      p(strong("Whole menstrual fluid")," — unfractionated collected material; ",strong("Menstrual-fluid cells")," — cells isolated directly before culture; ",strong("Tissue fragments")," — tissue recovered from menstrual fluid; ",strong("Cell-free menstrual fluid")," — soluble/supernatant fraction; ",strong("Cultured menstrual-derived cells")," — cells expanded ex vivo."))),
    uiOutput("plan_summary"),
    div(class="workspace-toolbar",
      tags$button(type="button",class="btn btn-sm btn-outline-secondary reset-workspace","Reset workspace"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_exact","+ Exact evidence"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_adjacent","+ Adjacent evidence"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_exact","+ Methods"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_experts_panel","+ Experts"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_data_panel","+ Data"),
      tags$button(type="button",class="btn btn-sm btn-outline-secondary restore-panel d-none",`data-target`="plan_translation","+ Companies")
    ),
    div(class="planner-workspace planner-workspace-columns",
      div(class="planner-column",
        workspace_panel("plan_exact","Exact evidence",uiOutput("plan_design_snapshot"),DTOutput("plan_table")),
        workspace_panel("plan_experts_panel","Relevant experts",DTOutput("plan_all_experts"))
      ),
      div(class="planner-column",
        workspace_panel("plan_data_panel","Reusable datasets",DTOutput("plan_datasets")),
        workspace_panel("plan_translation","Companies with overlap",uiOutput("plan_companies"))
      )
    )
  ),

  nav_panel("Contribute",
    div(class="page-intro compact-intro",h2("Contribute to MFOX"),p("Help identify menstrual-fluid evidence that conventional searches can miss. Every submission enters a curator review queue before it can change the public MFOX evidence base.")),
    section_overview(
      "Suggest missing or incorrect menstrual-fluid resources for curator review.",
      "Publications · Datasets · Projects · Companies · Researchers/labs · Corrections · Other resources.",
      "Choose the contribution type, add the strongest identifier or URL you have, then explain where menstrual fluid is used."
    ),
    div(class="contribute-shell",
      div(class="contribute-guide",
        h3("What would you like to contribute?"),
        selectInput("contrib_type","Contribution type",c("Publication / manuscript","Dataset","Project / consortium / research program","Company / technology","Researcher / lab","Correction to MFOX","Other menstrual-fluid resource")),
        p(class="muted small","A DOI, PMID, accession, company website or institutional profile is ideal, but not required if you can explain where menstrual fluid appears in the work.")),
      div(class="contribute-form",
        textInput("contrib_title","Title, resource or company name"),
        textInput("contrib_identifier","DOI, PMID, accession or URL"),
        selectInput("contrib_term","What does the source call the sample?",c("Not specified","Menstrual fluid","Menstrual blood","Menstrual effluent","Menstrual-derived cells","Endometrial cells / tissue","Uterine sample","Other terminology")),
        textAreaInput("contrib_mf_use","Where/how is menstrual fluid used?",rows=4,placeholder="Especially useful when menstrual fluid is not obvious from the title or abstract. Describe collection, specimen or sample processing if known."),
        textInput("contrib_assoc","Associated publication / dataset / company (optional)"),
        textAreaInput("contrib_notes","Anything else the curator should know?",rows=3),
        layout_columns(col_widths=c(6,6),textInput("contrib_name","Your name (optional)"),textInput("contrib_contact","ORCID / professional profile (optional)")),
        div(class="notification-optin",h5("Keep me updated about this submission"),textInput("contrib_email","Email address (optional)"),checkboxInput("contrib_notify","Email me when the MFOX curation team makes a decision on this submission",FALSE),p(class="muted small","Your email is kept in the private submission queue and is never added to the public MFOX evidence tables.")),
        actionButton("submit_contribution","Submit for curator review",class="btn-primary"),
        uiOutput("contrib_status")
      )
    ),
    div(class="contribute-explain",h3("What happens next?"),div(class="contribute-steps",span(strong("1")," Submitted"),span(strong("2")," Curator verifies menstrual-fluid relevance"),span(strong("3")," Accepted / needs information / out of scope"),span(strong("4")," Accepted records enter the appropriate MFOX table")),p(class="muted small","Community submissions complement publication searches, repository searches, citation expansion and researcher-based discovery. They never overwrite curated evidence automatically."))
  ),

  nav_panel("New & Unreviewed",
    div(class="discovery-shell",
      div(class="discovery-hero",
        div(class="discovery-icon",icon("binoculars")),
        div(h2("New & Unreviewed"),p("Candidates discovered by the MFOX engine before scientific curation.")),
        div(class="engine-badge",icon("rotate"),span("Weekly discovery"))
      ),
      section_overview(
        "See newly discovered candidate evidence before curator inclusion.",
        "Automated-search candidates · Community review-priority votes · Current triage state.",
        "Open a candidate, check the source, then vote only on review priority — not scientific inclusion."
      ),
      div(class="engine-card",
        div(class="engine-main",h3("Automated search by the MFOX engine"),p("The current automated workflow searches PubMed every 7 days. New records are added to the candidate queue only; a Fox must review them before they enter curated MFOX evidence."),p(class="engine-note",icon("calendar-days")," Scheduled: every Monday at 06:00 UTC · Repository-first discovery (GEO/SRA/PRIDE/MetaboLights) is the next engine extension.")),
        div(class="engine-side",strong("Community triage"),span("Vote for or against review priority. Votes help the MFOX Den order its queue; they never determine inclusion."))
      ),
      uiOutput("candidate_vote_summary"),
      uiOutput("candidate_cards")
    )
  ),
  nav_panel("MFOX Den",
    conditionalPanel("!output.fox_authenticated_client",
      div(class="fox-den-entry",
        div(class="fox-den-entry-visual",tags$img(src="mfox_den.png",alt="MFOX Den illustration")),
        div(class="fox-den-entry-copy",
          span(class="fox-kicker","MFOX CURATION WORKSPACE"),
          h2("MFOX Den"),
          p("MFOX Den is the restricted curator area of MFOX. This is where incoming evidence is reviewed, structured into the MFOX data model, linked to studies, assays, datasets, projects and organizations, and maintained over time."),
          div(class="fox-den-role-grid",
            div(icon("magnifying-glass"),div(strong("Review"),span("Assess new and community-submitted records before they enter curated evidence."))),
            div(icon("diagram-project"),div(strong("Structure"),span("Map samples, assays, datasets and relationships explicitly rather than inferring them."))),
            div(icon("shield-halved"),div(strong("Maintain"),span("Keep provenance, curation status and evidence links consistent as MFOX evolves.")))
          )
        )
      ),
      div(class="fox-login-card fox-login-card-den",h3("Curator access"),p("Enter the curator password configured for this deployment."),passwordInput("fox_password","MFOX Den password"),actionButton("fox_login","Enter MFOX Den",class="btn-primary"),uiOutput("fox_login_message"),p(class="muted small","Prototype authentication: set the environment variable MFOX_FOX_PASSWORD."))
    ),
    conditionalPanel("output.fox_authenticated_client",
      div(class="fox-den-shell",
        div(class="fox-den-hero fox-den-hero-compact",
          div(class="fox-den-art fox-den-art-illustrated",tags$img(src="mfox_den.png",alt="MFOX Den illustration")),
          div(class="fox-den-title",span(class="fox-kicker","MFOX CURATION WORKSPACE"),h2("MFOX Den"),p("Review evidence · structure relationships · maintain the curated knowledgebase")),
          div(class="fox-den-utilities",uiOutput("fox_email_status"),actionButton("fox_send_pending_email","Send pending emails",class="btn-light btn-sm"),actionButton("fox_logout","Log out",class="btn-light btn-sm"),uiOutput("fox_email_message"))
        ),
        navset_card_tab(
          id="fox_den_tabs",
          nav_panel("Community submissions",
            tab_overview("inbox","Community submissions","Review public submissions before they can alter curated MFOX. Select a record, verify the evidence, then curate now, accept for later, request information or mark it out of scope."),
            div(class="fox-review-layout",
              div(class="fox-queue-panel",
                div(class="fox-panel-heading",div(h3("Review queue"),uiOutput("fox_queue_count")),p("Select one row. Accepted and out-of-scope records leave this queue immediately.")),
                DTOutput("fox_queue_table")
              ),
              div(class="fox-review-panel",
                conditionalPanel("!output.fox_has_selection",
                  div(class="fox-review-placeholder",icon("arrow-left"),h3("Select a submission"),p("Choose a row from the review queue. The review form will always reset to that submission's own saved values."))
                ),
                conditionalPanel("output.fox_has_selection",
                  uiOutput("fox_selected_summary"),
                  div(class="fox-form-grid",
                    selectizeInput("fox_sample","Canonical sample",choices=c("Not yet classified"="Not yet classified","Whole menstrual fluid"="Whole menstrual fluid","Menstrual-fluid cells"="Menstrual-fluid cells","Tissue fragments"="Tissue fragments","Mixed cells/tissue fraction — source did not separate"="Cells/tissue — not further separated","Cell-free menstrual fluid"="Cell-free menstrual fluid","Extracellular vesicles (EVs)"="Extracellular vesicles (EVs)","Cultured menstrual-derived cells"="Cultured menstrual-derived cells","Experimentally manipulated derivatives"="Experimentally manipulated derivatives"),selected=character(0),multiple=TRUE,options=list(plugins=list("remove_button"),placeholder="Select one or more sample types")),
                    selectizeInput("fox_context","Clinical context",choices=c("Not yet classified","Healthy/reference","Endometriosis","Infertility","Recurrent pregnancy loss","Dysmenorrhea","Contraception","Reproductive immunology","Mixed clinical cohorts","Other/mixed"),selected=character(0),multiple=TRUE,options=list(plugins=list("remove_button"),placeholder="Select one or more clinical contexts")),
                    div(class="fox-form-span",selectizeInput("fox_assay","Assay / technology (if applicable)",choices=sort(unique(na.omit(c(mfox$assays$omics_modality,mfox$assays$assay_family)))),selected=character(0),multiple=TRUE,options=list(plugins=list("remove_button"),placeholder="Select one or more technologies"))),
                    div(class="fox-form-span",textAreaInput("fox_notes","Curator notes / reason",value="",rows=3))
                  ),
                  div(class="fox-actions fox-actions-stable",
                    actionButton("fox_curate_direct","Curate & add to database",class="btn-primary"),
                    actionButton("fox_accept","Accept for later curation",class="btn-success"),
                    actionButton("fox_needs","Needs information",class="btn-warning"),
                    actionButton("fox_reject","Out of scope",class="btn-outline-danger")
                  ),
                  p(class="muted small fox-review-action-note","Use “Curate & add to database” when you can complete the structured record now. “Accept for later curation” keeps the item visible as accepted but not yet mapped."),
                  uiOutput("fox_decision_status")
                )
              )
            )
          ),
          nav_panel("Community profiles",
            tab_overview("address-card","Community profiles","Review structured researcher, lab and company profiles submitted through Join the Community. These fields mirror the public directories so verified records can be promoted without re-entering the information."),
            p(class="muted","Use Add evidence → Researcher / lab or Company / technology to promote a verified profile into the curated database. Community submissions never publish automatically."),
            DTOutput("fox_community_profiles_table")
          ),
          nav_panel("Accepted",
            tab_overview("circle-check","Accepted","See submissions that passed initial review but may still need structured mapping into studies, assays, datasets, projects or organizations."),
            div(class="fox-accepted-intro",h3("Accepted submissions"),p("Everything accepted from Community submissions is listed here. Publications, datasets, projects/programs, companies and researcher/lab suggestions also appear in their public destination with a pending label until structured curation is complete.")),
            DTOutput("fox_accepted_table")
          ),
          nav_panel("Add evidence",
            tab_overview("database","Add evidence","Use direct curator entry when you already have verified information and can create explicit, structured relationships without going through the public submission queue."),
            div(class="fox-manual-shell",
              div(class="fox-manual-intro",
                div(span(class="fox-kicker","CURATOR DIRECT ENTRY"),h3("Add evidence directly to MFOX"),
                    p("For MFOX Den curators who already have verified evidence in hand. This bypasses the public Contribute queue and writes a structured record directly into the curated knowledgebase, preserving explicit links between studies, assays, datasets, projects and organizations.")),
                div(class="fox-manual-direct-badge",icon("database"),span("Direct curated write"))
              ),
              div(class="fox-manual-warning",
                icon("shield-halved"),
                div(strong("Use only after curator verification."),span("For multi-cohort or multi-assay papers, add one explicit sample–assay relationship at a time. MFOX will never infer cross-links between selected samples, cohorts and assays."))),
              uiOutput("fox_manual_origin_badge"),
              selectInput("fox_manual_resource","What are you adding?",
                choices=c("Publication + structured evidence"="evidence","Reusable dataset"="dataset","Project / consortium / program"="project","Company / technology"="company","Researcher / lab"="person"),
                selected="evidence"),
              conditionalPanel("input.fox_manual_resource == 'evidence'",
                radioButtons("fox_manual_mode","Entry type",
                  choices=c("New study / publication"="new","Add assay/sample to an existing study"="existing"),
                  selected="new",inline=TRUE),
                conditionalPanel("input.fox_manual_mode == 'existing'",
                  div(class="fox-manual-section",
                    h4("Existing study"),
                    selectizeInput("fox_manual_existing_study","Study",choices=setNames(mfox$studies$study_id,paste0(mfox$studies$study_id," · ",mfox$studies$year," · ",mfox$studies$title)),options=list(placeholder="Select a study")),
                    selectInput("fox_manual_existing_cohort","Cohort",choices=c("Create a new cohort"="__NEW__"))
                  )
                ),
                conditionalPanel("input.fox_manual_mode == 'new'",
                  div(class="fox-manual-section",
                    h4("Publication / study"),
                    div(class="fox-manual-grid",
                      div(class="fox-manual-span-2",textInput("fox_manual_title","Title *")),
                      numericInput("fox_manual_year","Year *",value=as.integer(format(Sys.Date(),"%Y")),min=1900,max=2100),
                      selectInput("fox_manual_pub_status","Publication status",c("Peer-reviewed","Preprint","Conference abstract","Other")),
                      textInput("fox_manual_doi","DOI"),
                      textInput("fox_manual_pmid","PMID"),
                      textInput("fox_manual_journal","Journal"),
                      textInput("fox_manual_first_author","First author"),
                      textInput("fox_manual_country","Country"),
                      div(class="fox-manual-span-2",textInput("fox_manual_source_url","Source URL")),
                      selectInput("fox_manual_pub_type","Publication type",c("Original research","Preprint","Conference abstract","Other")),
                      selectInput("fox_manual_design","Study design",c("Not reported","Cross-sectional","Case-control","Cohort","Longitudinal","Interventional","Methods / technical","Other")),
                      textInput("fox_manual_n_total","Total participants"),
                      selectInput("fox_manual_longitudinal","Longitudinal",c("No","Yes","Not reported"))
                    )
                  )
                ),
                conditionalPanel("input.fox_manual_mode == 'new' || input.fox_manual_existing_cohort == '__NEW__'",
                  div(class="fox-manual-section",
                    h4("Cohort"),
                    div(class="fox-manual-grid",
                      textInput("fox_manual_cohort_name","Cohort name",value="Main cohort"),
                      selectizeInput("fox_manual_context","Clinical context *",
                        choices=sort(unique(na.omit(mfox$cohorts$clinical_context))),options=list(placeholder="Select clinical context")),
                      textInput("fox_manual_condition","Condition / population detail"),
                      textInput("fox_manual_n_participants","Participants in this cohort"),
                      textInput("fox_manual_menstrual_day","Menstrual day / timing",value="NR"),
                      textInput("fox_manual_collection_device","Collection device",value="NR"),
                      selectInput("fox_manual_paired_blood","Paired peripheral blood",c("No","Yes","NR")),
                      selectInput("fox_manual_paired_endometrium","Paired endometrium",c("No","Yes","NR"))
                    )
                  )
                ),
                div(class="fox-manual-section",
                  h4("Sample + assay"),
                  div(class="fox-manual-grid",
                    selectInput("fox_manual_sample","Canonical sample *",choices=c(
                      "Whole menstrual fluid"="Whole menstrual fluid",
                      "Menstrual-fluid cells"="Menstrual-fluid cells",
                      "Tissue fragments"="Tissue fragments",
                      "Mixed cells/tissue fraction — source did not separate"="Cells/tissue — not further separated",
                      "Cell-free menstrual fluid"="Cell-free menstrual fluid",
                      "Extracellular vesicles (EVs)"="Extracellular vesicles",
                      "Cultured menstrual-derived cells"="Cultured menstrual-derived cells",
                      "Experimentally manipulated derivatives"="Experimentally manipulated derivatives")),
                    textInput("fox_manual_sample_fraction","Source wording / sample fraction"),
                    selectizeInput("fox_manual_assay_family","Assay family *",
                      choices=sort(unique(na.omit(mfox$assays$assay_family))),options=list(create=TRUE,placeholder="Select or type assay family")),
                    selectizeInput("fox_manual_modality","Assay modality *",
                      choices=sort(unique(na.omit(mfox$assays$omics_modality))),options=list(create=TRUE,placeholder="Select or type modality")),
                    textInput("fox_manual_assay_type","Assay type / method"),
                    textInput("fox_manual_platform","Platform"),
                    selectizeInput("fox_manual_domain","Research domain",
                      choices=sort(unique(na.omit(mfox$assays$research_domain))),options=list(create=TRUE,placeholder="Select or type domain")),
                    textInput("fox_manual_comparison","Primary comparison"),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_notes","Curator notes",rows=2))
                  )
                ),
                div(class="fox-manual-section fox-manual-dataset",
                  h4("Reusable dataset ",span(class="muted small","optional")),
                  div(class="fox-manual-grid",
                    textInput("fox_manual_repository","Repository"),
                    textInput("fox_manual_accession","Accession"),
                    textInput("fox_manual_dataset_url","Dataset URL"),
                    textInput("fox_manual_data_type","Data type"),
                    selectInput("fox_manual_raw","Raw data available",c("NR","Yes","No")),
                    selectInput("fox_manual_processed","Processed data available",c("NR","Yes","No")),
                    selectInput("fox_manual_metadata","Metadata available",c("NR","Yes","No")),
                    selectInput("fox_manual_code","Code available",c("NR","Yes","No"))
                  )
                )
              ),
              conditionalPanel("input.fox_manual_resource == 'dataset'",
                div(class="fox-manual-section",
                  h4("Attach dataset to curated assay"),
                  p(class="muted small","Choose the assay this dataset belongs to. This creates an explicit Dataset → Assay → Study link."),
                  selectizeInput("fox_manual_dataset_assay_id","Study / assay *",
                    choices=setNames(mfox$assays$assay_id,paste0(mfox$assays$study_id," · ",mfox$assays$assay_id," · ",mfox$assays$omics_modality," · ",sample_label(mfox$assays$biospecimen_class))),
                    options=list(placeholder="Select the assay represented by this dataset")),
                  div(class="fox-manual-grid",
                    textInput("fox_manual_ds_repository","Repository *"),
                    textInput("fox_manual_ds_accession","Accession *"),
                    div(class="fox-manual-span-2",textInput("fox_manual_ds_url","Dataset URL")),
                    textInput("fox_manual_ds_type","Data type"),
                    selectInput("fox_manual_ds_raw","Raw data available",c("NR","Yes","No")),
                    selectInput("fox_manual_ds_processed","Processed data available",c("NR","Yes","No")),
                    selectInput("fox_manual_ds_metadata","Metadata available",c("NR","Yes","No")),
                    selectInput("fox_manual_ds_code","Code available",c("NR","Yes","No")),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_ds_notes","Curator notes",rows=2))
                  )
                )
              ),
              conditionalPanel("input.fox_manual_resource == 'project'",
                div(class="fox-manual-section",
                  h4("Project / consortium / research program"),
                  p(class="muted small","Create the project as a first-class MFOX entity, then add only relationships you can verify. Leaving a study/company link blank is preferable to inferring one."),
                  div(class="fox-manual-grid",
                    textInput("fox_manual_project_name","Project / program name *"),
                    selectInput("fox_manual_project_type","Type",c("Academic research project","Consortium / network","Industry research program","Biobank / cohort","Atlas / reference resource","Other")),
                    textInput("fox_manual_project_lead","Lead organization *"),
                    textInput("fox_manual_project_country","Country"),
                    textInput("fox_manual_project_start","Start year"),
                    textInput("fox_manual_project_end","End year"),
                    selectInput("fox_manual_project_status","Status",c("Active","Completed","Planned / recruiting","Unknown")),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_project_focus","Focus / scope *",rows=2)),
                    div(class="fox-manual-span-2",textInput("fox_manual_project_website","Project website")),
                    div(class="fox-manual-span-2",textInput("fox_manual_project_source","Verification / source URL *")),
                    div(class="fox-manual-span-2",selectizeInput("fox_manual_project_studies","Verified linked studies",choices=setNames(mfox$studies$study_id,paste0(mfox$studies$study_id," · ",mfox$studies$year," · ",mfox$studies$title)),multiple=TRUE,options=list(placeholder="Optional — select only verified relationships"))),
                    div(class="fox-manual-span-2",selectizeInput("fox_manual_project_companies","Verified linked companies",choices=setNames(mfox$companies$company_id,paste0(mfox$companies$company_id," · ",mfox$companies$company)),multiple=TRUE,options=list(placeholder="Optional — select only verified relationships"))),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_project_notes","Curator notes",rows=2))
                  )
                )
              ),
              conditionalPanel("input.fox_manual_resource == 'company'",
                div(class="fox-manual-section",
                  h4("Company / technology"),
                  div(class="fox-manual-grid",
                    textInput("fox_manual_company_name","Company name *"),
                    textInput("fox_manual_company_website","Website *"),
                    textInput("fox_manual_company_city","City"),
                    textInput("fox_manual_company_country","Country"),
                    textInput("fox_manual_company_collection","Collection approach"),
                    selectInput("fox_manual_company_sample","Sample type",choices=c("Whole menstrual fluid","Menstrual-fluid cells","Tissue fragments","Cell-free menstrual fluid","Extracellular vesicles (EVs)","Cultured menstrual-derived cells","Other / mixed")),
                    div(class="fox-manual-span-2",textInput("fox_manual_company_technology","Technology / product *")),
                    div(class="fox-manual-span-2",textInput("fox_manual_company_application","Disease / application")),
                    div(class="fox-manual-span-2",selectizeInput("fox_manual_company_use","Utilization area(s)",
                      choices=c("Diagnostics & screening","Longitudinal monitoring","At-home testing & biosensors","Biomarker discovery","Collection & preanalytics","Research infrastructure","Fertility & reproductive health","Commercial testing","Regenerative medicine","Cell-derived products","Research tools & biobanking"),
                      multiple=TRUE,options=list(placeholder="Select all that apply"))),
                    div(class="fox-manual-span-2",selectizeInput("fox_manual_company_material","Material / component exploited",
                      choices=c("Whole menstrual fluid","Menstrual-fluid cells","Tissue fragments","Cell-free menstrual fluid","DNA / RNA","Proteins / hormones","Microbiome","Metabolites / lipids","Extracellular vesicles (EVs)","Cultured menstrual-derived cells","Other / mixed"),
                      multiple=TRUE,options=list(placeholder="Select all that apply"))),
                    selectInput("fox_manual_company_maturity","Standardized maturity",c("Early-stage feasibility","R&D / platform development","Translational / diagnostic development","Clinical / translational validation","Clinical validation","Research-use products / R&D","Commercial testing service","Regulatory-cleared / commercial platform","Other / unclear")),
                    selectInput("fox_manual_company_evidence","Evidence status",c("Company-reported / emerging","Company research program","Company-reported development","Company-reported platform","Commercial service documentation","Commercial research-use documentation","Public / institutional validation","Public innovation / feasibility project","Clinical validation study","Regulatory / clinical evidence","Peer-reviewed evidence","Other")),
                    textInput("fox_manual_company_stage","Detailed development stage / wording"),
                    textInput("fox_manual_company_source","Evidence / source URL"),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_company_note","Evidence note",rows=2))
                  )
                )
              ),
              conditionalPanel("input.fox_manual_resource == 'person'",
                div(class="fox-manual-section",
                  h4("Researcher / lab"),
                  div(class="fox-manual-grid",
                    textInput("fox_manual_person_name","Researcher or lab name *"),
                    textInput("fox_manual_person_institution","Institution *"),
                    textInput("fox_manual_person_city","City"),
                    textInput("fox_manual_person_country","Country"),
                    textInput("fox_manual_person_role","Role"),
                    textInput("fox_manual_person_orcid","ORCID"),
                    div(class="fox-manual-span-2",textInput("fox_manual_person_profile","Institutional / public profile URL *")),
                    div(class="fox-manual-span-2",textInput("fox_manual_person_expertise","Menstrual-fluid expertise / activity")),
                    div(class="fox-manual-span-2",textAreaInput("fox_manual_person_notes","Curator notes",rows=2))
                  )
                )
              ),
              div(class="fox-manual-actions",
                actionButton("fox_manual_save","Add to curated MFOX",class="btn-success"),
                actionButton("fox_manual_reset","Clear form",class="btn-outline-secondary"),
                uiOutput("fox_manual_status")
              ),
              tags$details(class="fox-manual-recent",open=FALSE,
                tags$summary("Recent manual additions"),
                DTOutput("fox_manual_log_table")
              )
            )
          ),
          nav_panel("Automated discovery",
            tab_overview("binoculars","Automated discovery","Inspect candidate publications and resources found by the discovery engine. Community voting from New & Unreviewed is summarized here to help order curator review; votes never determine inclusion."),
            div(class="fox-queue-intro",h3("Automated discovery"),p("Candidate publications/resources identified by the MFOX discovery engine, ordered with community triage context.")),
            uiOutput("fox_candidate_vote_summary"),
            DTOutput("fox_candidate_table")
          )
        ),
        tags$details(class="fox-history",tags$summary("Decision history"),DTOutput("fox_history_table"))
      )
    )
  ),
  nav_spacer(),
  nav_item(div(class="powered-brand",span("powered by"),tags$img(src="femmunityx_logo.png",alt="FemmunityX"))),
  nav_item(tags$span(class="version-chip",paste0("v1.4.19 · ",nrow(mfox$studies)," curated studies · ",nrow(mfox$registry_studies)," registered studies")))
)

server <- function(input,output,session){
  db_tick <- reactiveVal(0L)
  observe({
    db_tick()
    ev<-mfox$evidence
    pop_choices <- nz_choices(ev$clinical_context)
    omics_choices <- nz_choices(ev$omics_modality)
    sample_choices <- nz_choices(ev$biospecimen_class)
    current_pop <- isolate(input$explore_population)
    current_omics <- isolate(input$explore_omics)
    current_sample <- isolate(input$explore_biospecimen)
    updateSelectizeInput(session,"explore_population",choices=c("Any"="__ANY__",setNames(pop_choices,pop_choices)),selected=if(length(current_pop))current_pop else "__ANY__",server=TRUE)
    updateSelectizeInput(session,"explore_omics",choices=c("Any"="__ANY__",setNames(omics_choices,omics_choices)),selected=if(length(current_omics))current_omics else "__ANY__",server=TRUE)
    updateSelectizeInput(session,"explore_biospecimen",choices=c("Any"="__ANY__",setNames(sample_choices,sample_label(sample_choices))),selected=if(length(current_sample))current_sample else "__ANY__",server=TRUE)
    updateSelectInput(session,"plan_condition",choices=c("Any",nz_choices(ev$clinical_context)))
    updateSelectInput(session,"plan_omics",choices=c("Any",nz_choices(ev$omics_modality)))
    bs<-nz_choices(ev$biospecimen_class); updateSelectInput(session,"plan_biospecimen",choices=c("Any"="Any",setNames(bs,sample_label(bs))))
    updateSelectizeInput(session,"people_country",choices=nz_choices(mfox$people$country),server=TRUE)
    updateSelectizeInput(session,"community_company_country",choices=nz_choices(mfox$companies$country),server=TRUE)
    updateSelectizeInput(session,"community_company_use",choices=split_company_terms(mfox$companies$utilization_domain),server=TRUE)
    updateSelectizeInput(session,"map_technology",choices=nz_choices(ev$omics_modality),server=TRUE)
    updateSelectizeInput(session,"map_sample",choices=nz_choices(ev$biospecimen_class),server=TRUE)
    updateSelectizeInput(session,"map_disease",choices=nz_choices(ev$clinical_context),server=TRUE)
    updateSelectizeInput(session,"company_use",choices=split_company_terms(mfox$companies$utilization_domain),server=TRUE)
    updateSelectizeInput(session,"company_material",choices=split_company_terms(mfox$companies$material_exploited),server=TRUE)
    updateSelectizeInput(session,"company_maturity",choices=nz_choices(mfox$companies$maturity_level),server=TRUE)
    updateSelectizeInput(session,"company_evidence",choices=nz_choices(mfox$companies$evidence_status),server=TRUE)
  })
  observeEvent(input$go_landscape, bslib::nav_select("main_nav", selected="Landscape", session=session), ignoreInit=TRUE)
  observeEvent(input$home_landscape, bslib::nav_select("main_nav", selected="Landscape", session=session), ignoreInit=TRUE)
  observeEvent(input$go_plan, bslib::nav_select("main_nav", selected="Plan a Study", session=session), ignoreInit=TRUE)
  observeEvent(input$home_plan, bslib::nav_select("main_nav", selected="Plan a Study", session=session), ignoreInit=TRUE)
  observeEvent(input$home_community,bslib::nav_select("main_nav","Community",session=session),ignoreInit=TRUE)
  observeEvent(input$community_go_people,bslib::nav_select("community_tabs",selected="Researchers & labs",session=session),ignoreInit=TRUE)
  observeEvent(input$community_go_companies,bslib::nav_select("community_tabs",selected="Companies & innovators",session=session),ignoreInit=TRUE)
  observeEvent(input$community_go_groups,bslib::nav_select("community_tabs",selected="Collaboration groups",session=session),ignoreInit=TRUE)
  observeEvent(input$community_go_geo,bslib::nav_select("community_tabs",selected="Geography",session=session),ignoreInit=TRUE)
  observeEvent(input$community_go_join,bslib::nav_select("community_tabs",selected="Join the Community",session=session),ignoreInit=TRUE)
  observeEvent(input$community_open_companies,bslib::nav_select("main_nav",selected="Companies",session=session),ignoreInit=TRUE)
  observeEvent(input$home_data,{
    bslib::nav_select("main_nav",selected="Explore",session=session)
    bslib::nav_select("explore_tabs",selected="Datasets",session=session)
  },ignoreInit=TRUE)
  observeEvent(input$home_companies,bslib::nav_select("main_nav","Companies",session=session),ignoreInit=TRUE)
  observeEvent(input$home_contribute,bslib::nav_select("main_nav","Contribute",session=session),ignoreInit=TRUE)

  output$home_tech_layers<-renderUI({
    a<-mfox$assays |> filter(!is.na(assay_family),assay_family!="")
    x<-a |> group_by(assay_family) |> summarise(records=n_distinct(assay_id),methods=n_distinct(assay_type[!is.na(assay_type)&assay_type!=""]),.groups="drop") |> arrange(desc(records),assay_family)
    tech_icon<-function(z){z<-tolower(z); if(str_detect(z,"transcript|rna")) "file-waveform" else if(str_detect(z,"protein")) "circle-nodes" else if(str_detect(z,"metabol|lipid")) "flask" else if(str_detect(z,"genom|epigen|methyl")) "dna" else if(str_detect(z,"flow|cell")) "microscope" else if(str_detect(z,"microbi")) "bacteria" else "vial"}
    div(class="tech-grid compact-tech",lapply(seq_len(nrow(x)),function(i){
      fam<-x$assay_family[i]
      detail<-a |> filter(assay_family==fam) |> mutate(method=if_else(is.na(assay_type)|assay_type=="","Not specified",assay_type)) |> count(method,sort=TRUE)
      tip<-paste(c(paste0(fam," · ",x$records[i]," assay records"),"Methods represented in MFOX:",paste0("• ",detail$method," — ",detail$n),"","Click to inspect the underlying evidence"),collapse="\n")
      tags$button(type="button",class="tech-pill tech-home-link",`data-family`=fam,`data-tooltip`=tip,
        icon(tech_icon(fam)),div(strong(fam),span(paste0(x$records[i]," records · ",x$methods[i]," methods"))))
    }))
  })
  observeEvent(input$home_tech_click,{
    fam<-input$home_tech_click
    mods<-mfox$assays |> filter(assay_family==fam) |> pull(omics_modality) |> unique() |> na.omit()
    bslib::nav_select("main_nav","Explore",session=session)
    updateSelectizeInput(session,"explore_omics",selected=mods)
  },ignoreInit=TRUE)
  output$home_metrics<-renderUI({
    ev<-mfox$evidence; np<-n_distinct(mfox$people$person_id); nc<-n_distinct(mfox$people$country[!is.na(mfox$people$country)]); nd<-nrow(mfox$datasets); npr<-nrow(mfox$projects)
    div(class="snapshot-grid",div(strong(n_distinct(ev$study_id)),span("studies")),div(strong(n_distinct(ev$assay_id)),span("assays")),div(strong(nd),span("datasets")),div(strong(npr),span("projects/programs")),div(strong(np),span("authors")),div(strong(nc),span("countries")),div(strong(n_distinct(ev$biospecimen_class)),span("sample classes")))
  })

  scoped<-reactive({db_tick();apply_scope(mfox$evidence,input$scope)})
  landscape_long<-reactive({
    d<-scoped(); req(input$landscape_row!=input$landscape_col)
    d<-d |> mutate(.row=coalesce(as.character(.data[[input$landscape_row]]),"Not reported"),.col=coalesce(as.character(.data[[input$landscape_col]]),"Not reported"))
    if(input$landscape_row=="biospecimen_class") d$.row <- sample_label(d$.row)
    if(input$landscape_col=="biospecimen_class") d$.col <- sample_label(d$.col)
    if(isTRUE(input$landscape_transpose)) d<-d |> rename(.tmp=.row) |> mutate(.row=.col,.col=.tmp) |> select(-.tmp)
    d
  })
  landscape_counts<-reactive({
    d<-landscape_long()
    z<-if(input$landscape_measure=="studies") d |> distinct(study_id,.row,.col) |> count(.row,.col,name="n") else d |> distinct(assay_id,.row,.col) |> count(.row,.col,name="n")
    complete(z,.row,.col,fill=list(n=0))
  })
  output$landscape_kpis<-renderUI({d<-scoped(); div(class="metric-strip compact",div(strong(n_distinct(d$study_id)),span("studies")),div(strong(n_distinct(d$assay_id)),span("assays")),div(strong(n_distinct(d$omics_modality)),span("technologies")),div(strong(n_distinct(d$biospecimen_class)),span("sample classes")),div(strong(n_distinct(d$condition)),span("clinical contexts")))})
  output$landscape_title<-renderUI({tags$span(switch(input$landscape_view,heatmap="Evidence matrix",bubble="Bubble matrix",bars="Evidence composition",timeline="Evidence over time"))})
  output$landscape_view_guide<-renderUI({
    measure_lab <- if(input$landscape_measure=="studies") "study" else "assay"
    switch(input$landscape_view,
      heatmap=div(class="view-guide",icon("table-cells"),span(paste0("Each cell is the number of unique ",measure_lab," records represented by that row × column combination. One record can appear in more than one cell when it spans multiple categories."))),
      bubble=div(class="view-guide",icon("circle-dot"),span(paste0("Bubble size is the number of unique ",measure_lab," records in each combination. Bubbles are memberships, not mutually exclusive groups."))),
      bars=div(class="view-guide",icon("chart-column"),span(paste0("Colored segments show category memberships by row. If one ",measure_lab," spans multiple categories, segment totals are not a unique-record total."))),
      timeline=div(class="view-guide timeline-guide",icon("clock-rotate-left"),span(paste0("Background bars show the true number of unique ",measure_lab," records per year. Colored lines show how many of those records involve each selected category. Categories can overlap when a study contains multiple assays, samples or technologies, so the colored lines are intentionally not stacked.")))
    )
  })
  output$landscape_plot<-renderPlotly({
    d<-landscape_long(); z<-landscape_counts(); req(nrow(d)>0)
    measure_lab<-if(input$landscape_measure=="studies") "Studies" else "Assays"
    if(input$landscape_view=="timeline"){
      entity_col <- if(input$landscape_measure=="studies") "study_id" else "assay_id"
      t <- d |>
        transmute(entity=.data[[entity_col]],year=year,.col=.col) |>
        distinct(entity,year,.col) |>
        count(year,.col,name="n") |>
        mutate(year=suppressWarnings(as.numeric(as.character(year)))) |>
        filter(is.finite(year),!is.na(.col),.col!="")
      totals <- d |>
        transmute(entity=.data[[entity_col]],year=year) |>
        distinct(entity,year) |>
        count(year,name="total") |>
        mutate(year=suppressWarnings(as.numeric(as.character(year)))) |>
        filter(is.finite(year))
      validate(need(nrow(t)>0 && nrow(totals)>0,"No dated evidence is represented for the selected view."))
      pal <- mfox_category_palette(t$.col)
      ymax <- max(1,max(c(t$n,totals$total),na.rm=TRUE))
      totals <- totals |> mutate(tip=paste0("<b>Unique total</b><br>Year: ",year,"<br>",measure_lab,": ",total))
      t <- t |> mutate(tip=paste0("<b>",.col,"</b><br>Year: ",year,"<br>",measure_lab,": ",n))
      p<-ggplot()+
        geom_col(data=totals,aes(x=year,y=total,text=tip),width=.72,fill="#E9DEE2",color="#D8C9CE",linewidth=.25)+
        geom_line(data=t,aes(x=year,y=n,color=.col,group=.col,text=tip),linewidth=1.05,alpha=.9)+
        geom_point(data=t,aes(x=year,y=n,color=.col,text=tip),size=2.2)+
        scale_color_manual(values=pal)+
        scale_y_continuous(limits=c(0,ymax*1.08),breaks=scales::pretty_breaks(n=6),expand=expansion(mult=c(0,0)))+
        scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
        labs(x=NULL,y=measure_lab,color=NULL,subtitle="Background = unique yearly total · colored lines = overlapping category membership")+
        theme_minimal(base_size=12)+
        theme(legend.position="bottom",legend.box="horizontal",panel.grid.minor=element_blank(),
              panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(color="#EEE5E8",linewidth=.45),
              axis.line.y=element_line(color="#D8C9CE",linewidth=.4),plot.subtitle=element_text(color="#755E66",size=10),
              axis.title.y=element_text(face="bold",color="#5A3944"))
      return(ggplotly(p,tooltip="text") |>
        layout(legend=list(orientation="h",y=-.16),yaxis=list(rangemode="tozero",range=list(0,ymax*1.08),zeroline=TRUE,zerolinecolor="#CDBFC4"),
               margin=list(l=70,r=35,t=45,b=105),plot_bgcolor="#FFFFFF",paper_bgcolor="#FFFFFF") |>
        config(displaylogo=FALSE,responsive=TRUE))
    }
    if(input$landscape_sort=="density"){
      ro<-z |> group_by(.row) |> summarise(total=sum(n),.groups="drop") |> arrange(total,.row) |> pull(.row); co<-z |> group_by(.col) |> summarise(total=sum(n),.groups="drop") |> arrange(desc(total),.col) |> pull(.col)
    } else {ro<-rev(sort(unique(z$.row)));co<-sort(unique(z$.col))}
    z<-z |> mutate(.row=factor(.row,levels=ro),.col=factor(.col,levels=co),tip=paste0("<b>",.row,"</b><br>",.col,"<br>",measure_lab,": ",n))
    if(input$landscape_view=="heatmap"){
      mx<-max(1,max(z$n)); p<-ggplot(z,aes(.col,.row,fill=n,text=tip))+geom_tile(color="white",linewidth=1)+geom_text(aes(label=ifelse(n>0,n,"")),fontface="bold",size=4)+scale_fill_gradientn(colours=c("#F7F2F3","#F5C4B7","#E56D72","#B92F53","#65142F"),limits=c(0,mx))+labs(x=NULL,y=NULL,fill=measure_lab)+theme_minimal(base_size=12)+theme(axis.text.x=element_text(angle=35,hjust=1),panel.grid=element_blank())
    } else if(input$landscape_view=="bubble"){
      q<-z |> filter(n>0); validate(need(nrow(q)>0,"No represented combinations for these dimensions.")); p<-ggplot(q,aes(.col,.row,size=n,fill=n,text=tip))+geom_point(shape=21,color="#65142F",stroke=.7,alpha=.88)+scale_size(range=c(5,22))+scale_fill_gradient(low="#F5C4B7",high="#8D1E40")+labs(x=NULL,y=NULL,size=measure_lab,fill=measure_lab)+theme_minimal(base_size=12)+theme(axis.text.x=element_text(angle=35,hjust=1),panel.grid.minor=element_blank())
    } else {
      q<-z |> filter(n>0); validate(need(nrow(q)>0,"No represented combinations for these dimensions."))
      pal <- mfox_category_palette(q$.col)
      p<-ggplot(q,aes(.row,n,fill=.col,text=tip))+
        geom_col(position="stack",width=.72,color="white",linewidth=.2)+
        coord_flip()+scale_fill_manual(values=pal)+
        labs(x=NULL,y=measure_lab,fill=NULL)+theme_minimal(base_size=12)+
        theme(legend.position="bottom",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
              panel.grid.major.x=element_line(color="#EEE5E8",linewidth=.45),axis.title.x=element_text(face="bold",color="#5A3944"))
    }
    ggplotly(p,tooltip="text",dynamicTicks=TRUE) |> layout(autosize=TRUE,legend=list(orientation="h"),margin=list(l=150,r=35,t=30,b=150)) |> config(displaylogo=FALSE,responsive=TRUE)
  })
  output$landscape_studies<-renderDT({
    d<-scoped() |> mutate(Sample=sample_label(biospecimen_class),Paper=link_or_text(paste0(year," · ",title),source_url)) |> select(Paper,condition,Technology=omics_modality,Sample,`Assay family`=assay_family,`Research domain`=research_domain) |> distinct()
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE))
  })

  explore_specific_values <- function(x) {
    x <- as.character(x %||% character())
    setdiff(x[nzchar(x)], "__ANY__")
  }
  explore_filtered<-reactive({
    d<-scoped()
    pop <- explore_specific_values(input$explore_population)
    omics <- explore_specific_values(input$explore_omics)
    sample <- explore_specific_values(input$explore_biospecimen)
    if(length(pop)) d<-d|>filter(clinical_context%in%pop)
    if(length(omics)) d<-d|>filter(omics_modality%in%omics)
    if(length(sample)) d<-d|>filter(biospecimen_class%in%sample)
    if(isTRUE(input$public_only)) d<-d|>filter(assay_id%in%mfox$datasets$assay_id)
    d
  })
  explore_filters_active <- reactive({
    length(explore_specific_values(input$explore_population))>0 ||
      length(explore_specific_values(input$explore_omics))>0 ||
      length(explore_specific_values(input$explore_biospecimen))>0 ||
      isTRUE(input$public_only)
  })
  observeEvent(input$clear_filters,{
    updateSelectizeInput(session,"explore_population",selected="__ANY__")
    updateSelectizeInput(session,"explore_omics",selected="__ANY__")
    updateSelectizeInput(session,"explore_biospecimen",selected="__ANY__")
    updateCheckboxInput(session,"public_only",value=FALSE)
  })

  registry_filtered <- reactive({
    d <- mfox$registry_studies
    pop <- explore_specific_values(input$explore_population)
    omics <- explore_specific_values(input$explore_omics)
    sample <- explore_specific_values(input$explore_biospecimen)
    if(!is.null(input$registry_source_filter) && input$registry_source_filter!="__ALL__") d <- d |> filter(registry==input$registry_source_filter)
    if(length(pop)) d <- d |> filter(clinical_context %in% pop)
    if(length(omics)) d <- d[company_term_match(d$technology,omics),,drop=FALSE]
    if(length(sample)) d <- d[company_term_match(d$sample_type,sample),,drop=FALSE]
    if(isTRUE(input$public_only)){
      ds_studies <- unique(as.character(mfox$datasets$study_id))
      d <- d |> filter(!is.na(linked_study_id),linked_study_id!="",linked_study_id %in% ds_studies)
    }
    d
  })

  output$registry_study_metrics <- renderUI({
    d <- registry_filtered()
    active <- d |> filter(str_detect(tolower(coalesce(status,"")),"recruit|active|enrolling"))
    linked <- d |> filter(!is.na(linked_study_id),linked_study_id!="")
    div(class="metric-strip registry-metric-strip",
      div(strong(nrow(d)),span("registry records")),
      div(strong(n_distinct(d$registry[!is.na(d$registry)&d$registry!=""])),span("registry sources")),
      div(strong(nrow(active)),span("ongoing / upcoming")),
      div(strong(n_distinct(d$clinical_context[!is.na(d$clinical_context)&d$clinical_context!=""])),span("clinical contexts")),
      div(strong(nrow(linked)),span("linked to curated MFOX evidence"))
    )
  })



  output$registry_sources_table <- renderDT({
    counts <- mfox$registry_studies |> count(registry,name="Curated records")
    d <- mfox$registry_sources |> left_join(counts,by="registry") |>
      mutate(`Curated records`=coalesce(`Curated records`,0L),Source=link_or_text(registry,website)) |>
      transmute(Source,Region=region,Type=source_type,`Search route`=search_route,Priority=priority,`Curated records`,Notes=notes)
    datatable(d,escape=FALSE,rownames=FALSE,options=list(pageLength=16,scrollX=TRUE,dom="tip"))
  })

  output$explore_registry_table <- renderDT({
    d <- registry_filtered()
    if(!nrow(d)) return(datatable(tibble::tibble(Status="No registered studies match the current Explore filters."),rownames=FALSE,options=list(dom="t")))
    linked <- mfox$studies |> select(study_id,title,year,study_source_url=source_url) |>
      mutate(`Linked MFOX evidence`=link_or_text(paste0(year," · ",title),study_source_url)) |>
      select(linked_study_id=study_id,`Linked MFOX evidence`)
    d <- d |> left_join(linked,by="linked_study_id") |>
      mutate(Study=link_or_text(title,source_url),
             Registry=link_or_text(paste0(registry," · ",registry_id),source_url),
             `Other registration`=ifelse(!is.na(other_registrations)&other_registrations!="",link_or_text(other_registrations,other_registry_url),"—"),
             Technology=ifelse(is.na(technology)|technology=="","Not specified in registry",technology),
             Sample=ifelse(is.na(sample_type)|sample_type=="","Not specified",sample_type),
             `MF role`=coalesce(menstrual_fluid_role,"Not specified"),
             `Linked MFOX evidence`=coalesce(`Linked MFOX evidence`,"—")) |>
      transmute(Registry,`Other registration`,Study,Status=status,Context=clinical_context,Condition=condition,Type=study_type,Phase=phase,Enrollment=enrollment,Sponsor=sponsor,Country=country,Sample,Technology,`MF role`,`MF relevance`=mf_relevance,`Linked MFOX evidence`)
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(0,1,2,5,9,11,12,13,14,15)))))
  })

  project_summary_for_studies <- function(study_ids){
    ids<-unique(as.character(study_ids)); ids<-ids[!is.na(ids)&nzchar(ids)]
    if(!length(ids) || !nrow(mfox$project_studies)) return(tibble::tibble(study_id=character(),Projects=character()))
    links <- mfox$project_studies |> filter(as.character(study_id)%in%ids)
    if(!nrow(links)) return(tibble::tibble(study_id=character(),Projects=character()))
    links |>
      left_join(mfox$projects |> select(project_id,project_name,website,project_source_url=source_url),by="project_id") |>
      mutate(.url=coalesce(website,project_source_url),.link=link_or_text(project_name,.url)) |>
      group_by(study_id) |> summarise(Projects=paste(unique(.link),collapse="<br>"),.groups="drop")
  }

  output$explore_graph_metrics<-renderUI({
    db_tick(); ef<-explore_filtered(); sids<-unique(as.character(ef$study_id)); aids<-unique(as.character(ef$assay_id))
    ds<-mfox$datasets |> filter(as.character(assay_id)%in%aids)
    ps<-mfox$project_studies |> filter(as.character(study_id)%in%sids)
    proj_ids<-unique(as.character(ps$project_id)); proj_ids<-proj_ids[!is.na(proj_ids)&nzchar(proj_ids)]
    pubs<-mfox$community_publications |> filter(as.character(mfox_study_id)%in%sids)
    auth<-mfox$authorships |> filter(publication_id%in%pubs$publication_id)
    people<-mfox$people |> filter(person_id%in%auth$person_id)
    div(class="knowledge-strip",
      div(class="knowledge-node",icon("flask-vial"),div(strong(n_distinct(sids)),span("studies"))),
      div(class="knowledge-edge","→"),
      div(class="knowledge-node",icon("microscope"),div(strong(n_distinct(aids)),span("assays"))),
      div(class="knowledge-edge","↔"),
      div(class="knowledge-node",icon("file-lines"),div(strong(n_distinct(pubs$publication_id)),span("papers"))),
      div(class="knowledge-edge","↔"),
      div(class="knowledge-node",icon("database"),div(strong(n_distinct(ds$dataset_id)),span("datasets"))),
      div(class="knowledge-edge","↔"),
      div(class="knowledge-node",icon("diagram-project"),div(strong(length(proj_ids)),span("linked projects"))),
      div(class="knowledge-edge","↔"),
      div(class="knowledge-node",icon("building-columns"),div(strong(n_distinct(people$institution[!is.na(people$institution)&people$institution!=""])),span("linked institutions")))
    )
  })

  output$explore_connection_map<-renderPlotly({
    db_tick(); ef<-explore_filtered()
    validate(need(nrow(ef)>0,"No curated evidence matches the current filters."))
    flow <- ef |>
      mutate(Sample=sample_label(biospecimen_class),Technology=coalesce(omics_modality,"Technology not reported")) |>
      filter(!is.na(Sample),Sample!="",!is.na(Technology),Technology!="")
    sample_tech <- flow |>
      group_by(Sample,Technology) |>
      summarise(value=n_distinct(assay_id),.groups="drop") |>
      transmute(source=paste0("sample:",Sample),target=paste0("tech:",Technology),value,
                detail=paste0(Sample," → ",Technology," · ",value," assay",ifelse(value==1,"","s")))
    tech_study <- flow |> distinct(Technology,study_id)
    tech_assay <- flow |> distinct(Technology,assay_id)
    sids <- unique(as.character(ef$study_id)); aids <- unique(as.character(ef$assay_id))
    pubs <- mfox$community_publications |> filter(as.character(mfox_study_id)%in%sids)
    ds <- mfox$datasets |> filter(as.character(assay_id)%in%aids)
    ps <- mfox$project_studies |> filter(as.character(study_id)%in%sids)
    proj_ids <- unique(as.character(ps$project_id)); proj_ids<-proj_ids[!is.na(proj_ids)&nzchar(proj_ids)]
    auth <- mfox$authorships |> filter(publication_id%in%pubs$publication_id)
    people <- mfox$people |> filter(person_id%in%auth$person_id)

    paper_links <- tech_study |>
      inner_join(pubs |> select(mfox_study_id,publication_id),by=c("study_id"="mfox_study_id")) |>
      group_by(Technology) |> summarise(value=n_distinct(publication_id),.groups="drop") |>
      transmute(source=paste0("tech:",Technology),target="entity:Papers",value,
                detail=paste0(Technology," → Papers · ",value," linked paper",ifelse(value==1,"","s")))
    dataset_links <- tech_assay |>
      inner_join(ds |> select(assay_id,dataset_id),by="assay_id") |>
      group_by(Technology) |> summarise(value=n_distinct(dataset_id),.groups="drop") |>
      transmute(source=paste0("tech:",Technology),target="entity:Reusable datasets",value,
                detail=paste0(Technology," → Reusable datasets · ",value," dataset",ifelse(value==1,"","s")))
    project_links <- tech_study |>
      inner_join(ps |> select(study_id,project_id),by="study_id") |>
      group_by(Technology) |> summarise(value=n_distinct(project_id),.groups="drop") |>
      transmute(source=paste0("tech:",Technology),target="entity:Projects / programs",value,
                detail=paste0(Technology," → Projects / programs · ",value," linked project",ifelse(value==1,"","s")))
    institution_links <- tech_study |>
      inner_join(pubs |> select(mfox_study_id,publication_id),by=c("study_id"="mfox_study_id")) |>
      inner_join(auth |> select(publication_id,person_id),by="publication_id") |>
      inner_join(people |> select(person_id,institution),by="person_id") |>
      filter(!is.na(institution),institution!="") |>
      group_by(Technology) |> summarise(value=n_distinct(institution),.groups="drop") |>
      transmute(source=paste0("tech:",Technology),target="entity:Institutions",value,
                detail=paste0(Technology," → Institutions · ",value," linked institution",ifelse(value==1,"","s")))
    company_links <- tech_study |>
      inner_join(ps |> select(study_id,project_id),by="study_id") |>
      inner_join(mfox$project_companies |> select(project_id,company_id),by="project_id") |>
      group_by(Technology) |> summarise(value=n_distinct(company_id),.groups="drop") |>
      transmute(source=paste0("tech:",Technology),target="entity:Companies",value,
                detail=paste0(Technology," → Companies · ",value," linked compan",ifelse(value==1,"y","ies")))

    resource_links <- bind_rows(paper_links,dataset_links,project_links,institution_links,company_links)
    links <- bind_rows(sample_tech,resource_links) |> filter(value>0)
    validate(need(nrow(links)>0,"No evidence connections are available for this filter combination."))
    nodes <- tibble::tibble(id=unique(c(links$source,links$target))) |>
      mutate(label=sub("^[^:]+:","",id),kind=sub(":.*$","",id),
             color=dplyr::recode(kind,sample="#E98567",tech="#B83254",entity="#2F7E83",.default="#8797A5"))
    idx <- setNames(seq_len(nrow(nodes))-1,nodes$id)
    plot_ly(type="sankey",orientation="h",arrangement="snap",
      node=list(label=nodes$label,color=nodes$color,pad=18,thickness=18,line=list(color="rgba(80,30,45,.12)",width=1),
                hovertemplate="<b>%{label}</b><extra></extra>"),
      link=list(source=unname(idx[links$source]),target=unname(idx[links$target]),value=links$value,
                customdata=links$detail,color="rgba(143,29,57,.16)",
                hovertemplate="%{customdata}<extra></extra>")) |>
      layout(margin=list(l=10,r=10,t=8,b=8),font=list(size=11,color="#4f3540"))
  })

  output$explore_evidence_guidance<-renderUI({
    ef <- explore_filtered()
    if(!explore_filters_active()){
      return(div(class="explore-active-guide explore-full-guide",
        icon("database"),
        span(paste0("Showing the full curated evidence base · ",n_distinct(ef$study_id)," studies · ",n_distinct(ef$assay_id)," assays. Use the filters above whenever you want to narrow this view."))))
    }
    if(!nrow(ef)){
      return(div(class="explore-start-guide explore-no-match",
        div(class="explore-start-icon",icon("magnifying-glass")),
        div(class="explore-start-copy",
          h3("No curated study matches this combination yet"),
          p("This can mean the combination has not been represented in MFOX, or that the current filters are too narrow."),
          div(class="explore-start-steps",
            div(strong("Broaden one dimension"),span("Set Context, Technology or Sample back to Any.")),
            div(strong("Check dataset restriction"),span("Turn off “Only records with an indexed public dataset” if it is selected.")),
            div(strong("Treat the gap as information"),span("An empty intersection can identify an underrepresented study space rather than a search error."))
          ))))
    }
    div(class="explore-active-guide",icon("circle-check"),span(paste0(n_distinct(ef$study_id)," studies · ",n_distinct(ef$assay_id)," assays match the current Explore definition.")))
  })

  output$explore_table<-renderDT({
    ef<-explore_filtered(); proj<-project_summary_for_studies(ef$study_id)
    d<-ef |>
      left_join(proj,by="study_id") |>
      mutate(Paper=link_or_text(title,source_url),Sample=sample_label(biospecimen_class),
             `Source material wording`=coalesce(sample_fraction,""),
             `Assay method`=coalesce(assay_type,""),
             Projects=coalesce(Projects,""),
             `Reusable data`=ifelse(assay_id%in%mfox$datasets$assay_id,"Dataset linked","")) |>
      select(year,Paper,condition,Technology=omics_modality,`Assay method`,Sample,`Source material wording`,Projects,`Reusable data`,platform,longitudinal) |>
      distinct()
    datatable(d,escape=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(1,4,5,6,7)))))
  })

  output$explore_papers_table<-renderDT({
    db_tick(); ef<-explore_filtered(); study_ids<-unique(as.character(ef$study_id))
    ds_links <- mfox$datasets |>
      filter(as.character(study_id) %in% study_ids) |>
      mutate(.link=ifelse(!is.na(dataset_url)&dataset_url!="",
        paste0('<a href="',dataset_url,'" target="_blank" rel="noopener">',ifelse(is.na(accession)|accession=="","dataset",accession),' ↗</a>'),
        ifelse(is.na(accession),"",as.character(accession)))) |>
      group_by(study_id) |> summarise(Datasets=paste(unique(.link[nzchar(.link)]),collapse="<br>"),.groups="drop")
    projects<-project_summary_for_studies(study_ids)
    assay_summary <- ef |> group_by(study_id) |>
      summarise(`Evidence mapped`=paste0(n_distinct(assay_id)," assay",ifelse(n_distinct(assay_id)==1,"","s")," · ",n_distinct(biospecimen_class)," sample type",ifelse(n_distinct(biospecimen_class)==1,"","s")),.groups="drop")
    curated <- mfox$community_publications |>
      filter(as.character(mfox_study_id) %in% study_ids) |>
      left_join(ds_links,by=c("mfox_study_id"="study_id")) |>
      left_join(projects,by=c("mfox_study_id"="study_id")) |>
      left_join(assay_summary,by=c("mfox_study_id"="study_id")) |>
      mutate(Paper=link_or_text(ifelse(is.na(year),title,paste0(year," · ",title)),source_url),
             Identifier=coalesce(as.character(doi),as.character(pmid),""),Datasets=coalesce(Datasets,""),Projects=coalesce(Projects,""),Status="Curated") |>
      transmute(Paper,Identifier,Journal=journal,`Evidence mapped`,Datasets,`Projects / programs`=Projects,Status)
    if(!explore_filters_active()){
      acc <- accepted_publications()
      if(nrow(acc)){
        acc <- acc |> mutate(Paper=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),
          Identifier=coalesce(identifier_or_url,""),Journal="",`Evidence mapped`="Not yet added to structured evidence",Datasets="",`Projects / programs`="",Status="Accepted by curator · curation pending") |>
          select(Paper,Identifier,Journal,`Evidence mapped`,Datasets,`Projects / programs`,Status)
        curated <- bind_rows(curated,acc) |> distinct(Paper,.keep_all=TRUE)
      }
    }
    datatable(curated,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(0,3,4,5)))))
  })

  output$explore_datasets_table<-renderDT({
    db_tick(); ef<-explore_filtered(); ids<-unique(as.character(ef$assay_id))
    assay_meta <- ef |> distinct(assay_id,study_id,year,title,source_url,omics_modality,biospecimen_class)
    pub_by_study <- mfox$community_publications |>
      filter(!is.na(mfox_study_id),mfox_study_id!="") |>
      mutate(.paper=mapply(function(tt,yy,uu){lab<-paste0(ifelse(is.na(yy),"",paste0(yy," · ")),tt);link_or_text(lab,uu)},title,year,source_url,USE.NAMES=FALSE)) |>
      group_by(mfox_study_id) |> summarise(Paper=paste(unique(.paper),collapse="<br>"),.groups="drop") |>
      transmute(study_id=as.character(mfox_study_id),Paper)
    projects<-project_summary_for_studies(unique(as.character(ef$study_id)))
    d <- mfox$datasets |> filter(as.character(assay_id) %in% ids) |>
      left_join(assay_meta,by=c("assay_id","study_id")) |>
      left_join(pub_by_study,by="study_id") |>
      left_join(projects,by="study_id") |>
      mutate(Paper=coalesce(Paper,link_or_text(title,source_url)),Sample=sample_label(biospecimen_class),Projects=coalesce(Projects,""),
             Accession=link_or_text(ifelse(is.na(accession)|accession=="","Open dataset",accession),dataset_url),Status="Curated") |>
      select(Paper,Technology=omics_modality,Sample,Repository=repository,Accession,`Projects / programs`=Projects,Raw=raw_available,Processed=processed_available,Metadata=metadata_available,Code=code_available,Status) |>
      distinct()
    if(!explore_filters_active()){
      acc <- accepted_datasets()
      if(nrow(acc)){
        acc <- acc |> mutate(Paper=ifelse(is.na(associated_publication),"",htmltools::htmlEscape(as.character(associated_publication))),Technology="",Sample="",Repository="",
          Accession=mapply(accepted_resource_link,ifelse(is.na(title_or_name)|title_or_name=="","Accepted dataset",title_or_name),identifier_or_url,USE.NAMES=FALSE),
          `Projects / programs`="",Raw="",Processed="",Metadata="",Code="",Status="Accepted by curator · assay/study link pending") |>
          select(Paper,Technology,Sample,Repository,Accession,`Projects / programs`,Raw,Processed,Metadata,Code,Status)
        d <- bind_rows(d,acc)
      }
    }
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(0,2,4,5,10)))))
  })

  output$explore_projects_table<-renderDT({
    db_tick(); ef<-explore_filtered(); sids<-unique(as.character(ef$study_id))
    linked<-mfox$project_studies |> filter(as.character(study_id)%in%sids)
    if(explore_filters_active()) pids<-unique(as.character(linked$project_id)) else pids<-as.character(mfox$projects$project_id)
    pids<-pids[!is.na(pids)&nzchar(pids)]
    if(explore_filters_active() && !length(pids)){
      return(datatable(tibble::tibble(Status="No projects or programs have a verified relationship to the current filtered evidence."),rownames=FALSE,options=list(dom="t")))
    }
    ps<-mfox$project_studies |> filter(as.character(project_id)%in%pids) |>
      left_join(mfox$studies |> select(study_id,title,year,study_source_url=source_url),by="study_id") |>
      mutate(.paper=link_or_text(paste0(year," · ",title),study_source_url)) |>
      group_by(project_id) |> summarise(`Linked papers`=paste(unique(.paper),collapse="<br>"),`Study links`=n_distinct(study_id),.groups="drop")
    pc<-mfox$project_companies |> filter(as.character(project_id)%in%pids) |>
      left_join(mfox$companies |> select(company_id,company,website),by="company_id") |>
      mutate(.company=link_or_text(company,website)) |>
      group_by(project_id) |> summarise(`Linked companies`=paste(unique(.company),collapse="<br>"),.groups="drop")
    d<-mfox$projects |> filter(as.character(project_id)%in%pids) |>
      left_join(ps,by="project_id") |> left_join(pc,by="project_id") |>
      mutate(Project=link_or_text(project_name,coalesce(website,source_url)),Period=case_when(!is.na(start_year)&!is.na(end_year)~paste0(start_year,"–",end_year),!is.na(start_year)~paste0(start_year,"–"),TRUE~""),
             `Linked papers`=coalesce(`Linked papers`,""),`Linked companies`=coalesce(`Linked companies`,""),`Study links`=coalesce(`Study links`,0L)) |>
      transmute(Project,Type=project_type,Lead=lead_organization,Country=country,Period,Status=status,Focus=focus,`Linked papers`,`Linked companies`,`Study links`)
    acc<-accepted_projects()
    if(!explore_filters_active() && nrow(acc)){
      pa<-acc |> transmute(Project=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),Type="Community-submitted project/program",Lead="",Country="",Period="",Status="Accepted · curation pending",Focus=coalesce(mf_use_description,""),`Linked papers`="",`Linked companies`="",`Study links`=0L)
      d<-bind_rows(d,pa)
    }
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(0,6,7,8)))))
  })

  output$explore_labs_table<-renderDT({
    db_tick(); ef<-explore_filtered(); sids<-unique(as.character(ef$study_id))
    pubs<-mfox$community_publications |> filter(as.character(mfox_study_id)%in%sids)
    d<-mfox$authorships |> filter(publication_id%in%pubs$publication_id) |> left_join(mfox$people,by="person_id") |>
      filter(!is.na(institution),institution!="") |>
      group_by(institution,country) |> summarise(`Linked researchers`=n_distinct(person_id),`Linked papers`=n_distinct(publication_id),Researchers=paste(head(sort(unique(name)),6),collapse=" · "),.groups="drop") |>
      arrange(desc(`Linked papers`),desc(`Linked researchers`),institution) |>
      mutate(`Linked researchers`=as.character(`Linked researchers`),`Linked papers`=as.character(`Linked papers`))
    if(!explore_filters_active()){
      acc<-accepted_people_labs()
      if(nrow(acc)){
        pa<-acc |> transmute(institution=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),country="",`Linked researchers`="",`Linked papers`="",Researchers="Accepted by curator · profile curation pending")
        d<-bind_rows(d,pa)
      }
    }
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=c(0,4)))))
  })

  output$explore_companies_table<-renderDT({
    db_tick(); ef<-explore_filtered(); sids<-unique(as.character(ef$study_id))
    linked_projects<-mfox$project_studies |> filter(as.character(study_id)%in%sids) |> pull(project_id) |> unique()
    linked_company_ids<-mfox$project_companies |> filter(project_id%in%linked_projects) |> pull(company_id) |> unique()
    d<-mfox$companies
    if(explore_filters_active()) d<-d |> filter(company_id%in%linked_company_ids)
    pnames<-mfox$project_companies |> left_join(mfox$projects |> select(project_id,project_name,website,project_source_url=source_url),by="project_id") |>
      mutate(.project=link_or_text(project_name,coalesce(website,project_source_url))) |> group_by(company_id) |> summarise(Projects=paste(unique(.project),collapse="<br>"),.groups="drop")
    d<-d |> left_join(pnames,by="company_id") |> mutate(Company=link_or_text(company,website),Projects=coalesce(Projects,""),Status="Curated") |>
      transmute(Company,Location=mapply(function(a,b) paste(stats::na.omit(c(a,b)),collapse=", "),city,country,USE.NAMES=FALSE),
                Utilization=utilization_domain,Material=material_exploited,Technology=technology,Application=disease_application,
                `Development stage`=maturity_level,`Evidence status`=evidence_status,`Projects / programs`=Projects,Status)
    if(!explore_filters_active()){
      acc<-accepted_companies()
      if(nrow(acc)){
        pa<-acc |> transmute(Company=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),Location="",Utilization="",Material="",Technology="",Application=coalesce(mf_use_description,""),`Development stage`="",`Evidence status`="",`Projects / programs`="",Status="Accepted by curator · company curation pending")
        d<-bind_rows(d,pa)
      }
    }
    if(!nrow(d)) d<-tibble::tibble(Status="No companies have a verified relationship to the current filtered evidence.")
    datatable(d,escape=FALSE,rownames=FALSE,filter=if(ncol(d)>1)"top"else"none",options=list(pageLength=10,scrollX=TRUE,columnDefs=list(list(className="dt-wrap",targets=seq_len(max(1,ncol(d)))-1))))
  })

  plan_query <- eventReactive(input$plan_run,{
    vals <- list(
      condition=input$plan_condition %||% "Any",
      omics=input$plan_omics %||% "Any",
      biospecimen=input$plan_biospecimen %||% "Any",
      longitudinal=input$plan_longitudinal %||% "Any"
    )
    vals$active <- any(unlist(vals[c("condition","omics","biospecimen","longitudinal")])!="Any")
    vals
  },ignoreInit=FALSE,ignoreNULL=FALSE)
  plan_definition_active <- reactive(isTRUE(plan_query()$active))
  plan_results<-reactive({
    db_tick()
    q <- plan_query()
    if(!isTRUE(q$active)) return(mfox$evidence |> slice(0))
    d<-mfox$evidence
    if(q$condition!="Any")d<-d|>filter(str_detect(coalesce(clinical_context,""),fixed(q$condition)))
    if(q$omics!="Any")d<-d|>filter(omics_modality==q$omics)
    if(q$biospecimen!="Any")d<-d|>filter(biospecimen_class==q$biospecimen)
    if(q$longitudinal!="Any")d<-d|>filter(longitudinal==q$longitudinal)
    d
  })
  expert_ids<-reactive({ids<-unique(plan_results()$study_id);pubs<-mfox$community_publications|>filter(mfox_study_id%in%ids)|>pull(publication_id);mfox$authorships|>filter(publication_id%in%pubs)|>pull(person_id)|>unique()})
  output$plan_summary<-renderUI({
    d<-plan_results()
    if(!plan_definition_active()) return(div(class="planner-start-guide",icon("pen-ruler"),div(h3("Define the study you are considering"),p("Choose at least one specific feature above. The planner is intentionally targeted: leaving everything as Any would simply reproduce the field-wide Explore view."),p(class="muted","Example: Endometriosis + single-cell transcriptomics + menstrual-fluid cells. You can leave the dimensions you do not care about as Any."))))
    studies <- mfox$studies |> filter(study_id %in% d$study_id)
    cohorts <- mfox$cohorts |> filter(study_id %in% d$study_id)
    datasets <- mfox$datasets |> filter(assay_id %in% d$assay_id)
    participants <- sum(studies$n_mf,na.rm=TRUE); if(!is.finite(participants)) participants <- 0
    q <- plan_query()
    company_hits <- company_plan_overlap(mfox$companies,q)
    metric <- function(value,label,target,title) tags$button(type="button",class="intel-link",`data-target`=target,title=title,strong(value),span(label),tags$small("Open panel →"))
    div(class="study-intel",
      h3("Study intelligence"),
      p(class="intel-hint","This is a targeted precedent view for the study definition above. Select a metric to jump to the corresponding evidence panel."),
      div(class="metric-strip",
        metric(n_distinct(d$study_id),"exact studies","plan_exact","Open exact evidence"),
        metric(n_distinct(d$assay_id),"assays","plan_exact","Open methods and assay precedent"),
        metric(participants,"MF participants reported","plan_exact","Open studies reporting menstrual-fluid participants"),
        metric(nrow(datasets),"public datasets","plan_data_panel","Open reusable datasets"),
        metric(length(expert_ids()),"linked researchers","plan_experts_panel","Open relevant experts"),
        metric(nrow(company_hits),"companies with overlap","plan_translation","Open companies sharing the selected disease/context or technology family"),
        metric(sum(studies$longitudinal=="Yes",na.rm=TRUE),"longitudinal studies","plan_exact","Open study-design and methods precedent")
      ),
      if(!nrow(d)) div(class="empty-note","No exact MFOX match for this proposed study definition. That is useful planning information: broaden one dimension to identify the closest methodological or clinical precedent.")
    )
  })
  output$plan_design_snapshot<-renderUI({d<-plan_results();if(!plan_definition_active())return(div(class="empty-note","Your design snapshot will appear here after you define at least one study feature and build study intelligence."));if(!nrow(d))return(div(class="empty-note","No exact-match methods are represented for this proposed study definition. Broaden one dimension to find the nearest precedent."));s<-mfox$studies|>filter(study_id%in%d$study_id);c<-mfox$cohorts|>filter(study_id%in%d$study_id);a<-mfox$assays|>filter(assay_id%in%d$assay_id); line<-function(label,x) p(strong(paste0(label,": ")),if(length(x))paste(unique(x),collapse=" · ") else "Not consistently reported");tagList(line("Study designs",na.omit(s$study_design)),line("Collection devices",na.omit(c$collection_device[c$collection_device!="NR"])),line("Collection setting",na.omit(c$collection_setting[c$collection_setting!="NR"])),line("Processing time",na.omit(c$time_to_processing[c$time_to_processing!="NR"])),line("Fresh / frozen",na.omit(c$fresh_frozen[c$fresh_frozen!="NR"])),line("Platforms",na.omit(a$platform[a$platform!="NR"])),p(strong("Paired blood: "),sum(c$paired_blood=="Yes",na.rm=TRUE)," cohorts · ",strong("Paired endometrium: "),sum(c$paired_endometrium=="Yes",na.rm=TRUE)," cohorts"))})
  expert_records<-function(ids){
    pubs <- mfox$community_publications |> filter(mfox_study_id %in% ids)
    auth <- mfox$authorships |> filter(publication_id %in% pubs$publication_id) |> left_join(mfox$people,by="person_id")
    # Authorship-position fields remain available for relevance filtering/ranking,
    # but are intentionally not exposed as reader-facing columns.
    auth |> left_join(pubs |> select(publication_id,title,year,source_url),by="publication_id") |>
      mutate(profile=coalesce(public_profile_url,"")) 
  }
  output$plan_experts<-renderUI({
    x <- expert_records(unique(plan_results()$study_id))
    if(!nrow(x)) return(div(class="empty-note","No linked researchers are represented for the current study definition."))
    inst <- x |> mutate(institution=coalesce(institution,"Institution not reported"),country=coalesce(country,"")) |>
      group_by(institution,country) |> summarise(n_people=n_distinct(person_id),n_papers=n_distinct(publication_id),.groups="drop") |>
      arrange(desc(n_people),desc(n_papers),institution)
    tagList(
      div(class="expert-view-head",div(h3("Relevant experts"),p("Grouped by institution and shared menstrual-fluid publications. Open a group to see researchers, profile links and the papers connecting them to this evidence."))),
      div(class="expert-groups",lapply(seq_len(nrow(inst)),function(i){
        ii<-inst[i,]; g<-x |> filter(coalesce(institution,"Institution not reported")==ii$institution)
        papers<-g |> distinct(publication_id,title,year,source_url) |> arrange(desc(year))
        tags$details(class="expert-institution",open=i==1,
          tags$summary(div(class="expert-summary",div(h4(ii$institution),span(ii$country)),div(class="expert-counts",span(paste(ii$n_people,"researcher(s)")),span(paste(ii$n_papers,"linked paper(s)"))))),
          div(class="expert-people",lapply(split(g,g$person_id),function(z){
            z<-z[1,]; div(class="expert-person",
              div(class="expert-person-main",strong(z$name),span(class="muted",ifelse(is.na(z$institution),"",z$institution))),
              div(class="expert-links",
                if(!is.na(z$profile)&&nzchar(z$profile)) tags$a(href=z$profile,target="_blank",rel="noopener",icon("arrow-up-right-from-square")," Researcher profile") else span(class="muted","No external profile linked"),
                span(class="expert-role","MFOX-linked researcher")
              ))
          })),
          div(class="expert-papers",h5("Publications from this institution in the current evidence set"),lapply(seq_len(nrow(papers)),function(j){
            pp<-papers[j,]; div(class="expert-paper",icon("file-lines"),
              if(!is.na(pp$source_url)&&nzchar(pp$source_url)) tags$a(href=pp$source_url,target="_blank",rel="noopener",paste0(pp$year," · ",pp$title)) else span(paste0(pp$year," · ",pp$title)))
          }))
        )
      }))
    )
  })
  output$plan_table<-renderDT({plan_results()|>mutate(Paper=link_or_text(paste0(year," · ",title),source_url),Sample=sample_label(biospecimen_class))|>select(Paper,condition,Technology=omics_modality,Sample,study_design,longitudinal)|>distinct()|>datatable(escape=FALSE,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))})
  output$plan_all_experts<-renderDT({
    ids <- unique(plan_results()$study_id)
    pubs <- mfox$community_publications |> filter(mfox_study_id %in% ids)
    d <- mfox$authorships |> filter(publication_id %in% pubs$publication_id) |>
      left_join(mfox$people,by="person_id") |>
      left_join(pubs |> select(publication_id,title,year,source_url),by="publication_id") |>
      group_by(person_id,name,institution,country,public_profile_url) |>
      summarise(`MF papers`={
        links <- mapply(function(tt,yy,uu){
          label <- paste0(yy," · ",tt)
          if(!is.na(uu) && nzchar(uu)) paste0('<a href="',htmltools::htmlEscape(uu),'" target="_blank" rel="noopener">',htmltools::htmlEscape(label),'</a>') else htmltools::htmlEscape(label)
        },title,year,source_url,USE.NAMES=FALSE)
        paste(unique(links[nzchar(links)]),collapse="<br>")
      },.groups="drop") |>
      mutate(Researcher=link_or_text(name,public_profile_url),
             Institution=ifelse(is.na(institution)|institution=="","—",institution),
             Country=ifelse(is.na(country)|country=="","—",country)) |>
      select(Researcher,Institution,Country,`MF papers`) |>
      arrange(Researcher)
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE,dom='tip'))
  })
  output$plan_datasets<-renderDT({ids<-unique(plan_results()$assay_id);d<-mfox$datasets|>filter(assay_id%in%ids)|>mutate(Accession=link_or_text(accession,dataset_url))|>select(repository,Accession,raw_available,processed_available,metadata_available,code_available);datatable(d,escape=FALSE,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))})

  community_people<-reactive({db_tick();a<-mfox$authorships|>group_by(person_id)|>summarise(n_publications=n_distinct(publication_id),first=any(is_first_author=="Yes",na.rm=TRUE),last=any(is_last_author=="Yes",na.rm=TRUE),corresponding=any(is_corresponding_author=="Yes",na.rm=TRUE),equal=any(is_equal_contribution=="Yes",na.rm=TRUE),.groups="drop")|>mutate(recurring=n_publications>1,community_visible=first|last|corresponding|equal|recurring);d<-mfox$people|>left_join(a,by="person_id")|>mutate(n_publications=coalesce(n_publications,0L),community_visible=coalesce(community_visible,FALSE)|str_detect(coalesce(notes,""),fixed("Community-curated profile")));if(isTRUE(input$people_visible_only))d<-d|>filter(community_visible);if(length(input$people_country))d<-d|>filter(country%in%input$people_country);if(nzchar(trimws(input$people_search))){q<-tolower(trimws(input$people_search));d<-d|>filter(str_detect(tolower(paste(name,institution,city,country)),fixed(q)))};d})
  output$people_table<-renderDT({
    d <- community_people() |> mutate(Researcher=link_or_text(name,public_profile_url),Status="Curated") |>
      select(Researcher,institution,city,country,`MF publications`=n_publications,Status)
    acc <- accepted_people_labs()
    if(nrow(acc)){
      acc <- acc |> transmute(
        Researcher=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),
        institution="",city="",country="",`MF publications`="",Status="Accepted by curator · profile curation pending")
      d <- bind_rows(d,acc)
    }
    d |> arrange(desc(as.character(`MF publications`)),Researcher) |>
      datatable(escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })
  community_companies <- reactive({
    db_tick()
    d <- mfox$companies
    q <- trimws(input$community_company_search %||% "")
    if(nzchar(q)){
      hay <- paste(d$company,d$technology,d$disease_application,d$utilization_domain,d$country,d$city,sep=" | ")
      d <- d[grepl(q,hay,ignore.case=TRUE),,drop=FALSE]
    }
    if(length(input$community_company_country)) d <- d |> filter(country %in% input$community_company_country)
    if(length(input$community_company_use)) d <- d[company_term_match(d$utilization_domain,input$community_company_use),,drop=FALSE]
    d
  })
  output$community_summary_metrics <- renderUI({
    db_tick()
    pubs <- mfox$community_publications
    auth <- mfox$authorships
    people <- mfox$people
    groups <- auth |> left_join(people |> select(person_id,institution),by="person_id") |> filter(!is.na(institution),institution!="") |> distinct(institution)
    research_countries <- people$country[!is.na(people$country)&people$country!=""]
    company_countries <- mfox$companies$country[!is.na(mfox$companies$country)&mfox$companies$country!=""]
    div(class="metric-strip community-metric-strip",
      div(strong(nrow(groups)),span("research institutions")),
      div(strong(n_distinct(auth$person_id)),span("publication-linked researchers")),
      div(strong(n_distinct(pubs$publication_id)),span("curated publications")),
      div(strong(nrow(mfox$companies)),span("companies / innovators")),
      div(strong(length(unique(c(research_countries,company_countries)))),span("countries represented"))
    )
  })

  output$community_company_cards <- renderUI({
    d <- community_companies()
    if(!nrow(d)) return(div(class="empty-note","No companies match the current Community filters."))
    cards <- lapply(seq_len(nrow(d)),function(i){
      uses <- trimws(str_split(coalesce(d$utilization_domain[i],""),";")[[1]]); uses <- uses[nzchar(uses)]
      loc <- paste(na.omit(c(d$city[i],d$country[i])),collapse=", ")
      div(class="company-overview-card community-company-card",
        div(class="company-card-head",
          div(class="entity-heading",entity_mark(d$company[i]),div(h3(d$company[i]),div(class="company-location",icon("location-dot"),ifelse(nzchar(loc),loc,"Location not curated")))),
          if(!is.na(d$website[i])&&nzchar(d$website[i])) tags$a(href=d$website[i],target="_blank",rel="noopener",class="company-site",icon("arrow-up-right-from-square")," Website")
        ),
        if(length(uses)) div(class="company-use-chips",lapply(uses,function(tt) span(class="company-use-chip",company_icon(tt),tt))),
        if(!is.na(d$disease_application[i])&&nzchar(d$disease_application[i])) div(class="company-card-section",strong("Working on"),p(d$disease_application[i])),
        div(class="community-company-foot",span(icon("users"),"Community organization view"),span("Open Companies for evidence, maturity and utilization detail"))
      )
    })
    div(class="company-overview-grid community-company-grid",cards)
  })

  output$community_company_table<-renderDT({
    d <- community_companies() |> mutate(
      Company=link_or_text(company,website),
      Location=trimws(paste(ifelse(is.na(city),"",city),ifelse(is.na(country),"",country),sep=", ")),
      Location=gsub("^,[[:space:]]*|,[[:space:]]*$","",Location)
    ) |> transmute(Company,Location,Utilization=utilization_domain,Technology=technology,Application=disease_application,
                   `Development stage`=maturity_level,`Evidence status`=evidence_status)
    acc <- accepted_companies()
    if(nrow(acc)){
      acc <- acc |> transmute(
        Company=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),
        Location="",Utilization="",Technology="",Application="",`Development stage`="Pending structured curation",`Evidence status`="Accepted by curator")
      d <- bind_rows(d,acc)
    }
    if(!nrow(d)) d <- tibble::tibble(Status="No companies match the current Community filters.")
    datatable(d,escape=FALSE,rownames=FALSE,filter=if("Status" %in% names(d)) "none" else "top",options=list(pageLength=15,scrollX=TRUE))
  })
  output$collaboration_groups<-renderUI({
    db_tick()
    d<-mfox$authorships |>
      left_join(mfox$people,by="person_id") |>
      left_join(mfox$community_publications,by="publication_id") |>
      filter(!is.na(institution),institution!="") |>
      group_by(institution,city,country) |>
      summarise(n_people=n_distinct(person_id),n_pubs=n_distinct(publication_id),
                researchers=paste(head(sort(unique(name)),8),collapse=" · "),
                papers=paste(head(sort(unique(title)),4),collapse=" | "),.groups="drop") |>
      arrange(desc(n_pubs),desc(n_people))
    if(!nrow(d)) return(div(class="empty-note","No collaboration groups are represented in the current Community data."))
    div(class="company-overview-grid community-group-grid",lapply(seq_len(nrow(d)),function(i){
      loc <- paste(na.omit(c(d$city[i],d$country[i])),collapse=", ")
      div(class="company-overview-card community-group-card",
        div(class="company-card-head community-group-head",
          div(class="entity-heading",
            entity_mark(d$institution[i]),
            div(h3(d$institution[i]),div(class="company-location",icon("location-dot"),ifelse(nzchar(loc),loc,"Location not curated"))))
        ),
        div(class="community-group-stats",
          span(icon("file-lines"),strong(d$n_pubs[i])," MF publications"),
          span(icon("users"),strong(d$n_people[i])," researchers")
        ),
        div(class="company-card-section",strong("Researchers"),p(d$researchers[i])),
        tags$details(class="community-publications",tags$summary(icon("book-open")," Representative publications"),p(d$papers[i]))
      )
    }))
  })
  community_evidence<-reactive({mfox$authorships|>left_join(mfox$people|>select(person_id,name,institution,city,country,latitude,longitude,public_profile_url),by="person_id")|>left_join(mfox$community_publications|>select(publication_id,mfox_study_id,title,year,source_url),by="publication_id")|>left_join(mfox$evidence|>select(study_id,omics_modality,biospecimen_class,condition,population_category,research_domain)|>distinct(),by=c("mfox_study_id"="study_id"))|>mutate(technology=coalesce(omics_modality,"Not represented in MFOX Evidence"),sample_type=coalesce(biospecimen_class,"Not represented in MFOX Evidence"),disease_question=coalesce(condition,population_category,"Not reported"),domain=coalesce(research_domain,"Not reported"))})
  make_bipartite_plot<-function(edges,left_col,right_col,left_label,right_label){edges<-edges|>filter(!is.na(.data[[left_col]]),!is.na(.data[[right_col]]))|>count(.data[[left_col]],.data[[right_col]],name="weight");validate(need(nrow(edges)>0,"No connections match the current filters."));left<-sort(unique(edges[[left_col]]));right<-sort(unique(edges[[right_col]]));nodes<-bind_rows(tibble(label=left,x=0,y=seq_along(left),group=left_label),tibble(label=right,x=1,y=seq_along(right),group=right_label))|>group_by(group)|>mutate(y=(y-(max(y)+1)/2)/max(1,max(y)))|>ungroup();seg<-edges|>left_join(nodes|>filter(group==left_label)|>select(left=label,x0=x,y0=y),by=setNames("left",left_col))|>left_join(nodes|>filter(group==right_label)|>select(right=label,x1=x,y1=y),by=setNames("right",right_col));p<-plot_ly();for(i in seq_len(nrow(seg)))p<-p|>add_segments(x=seg$x0[i],y=seg$y0[i],xend=seg$x1[i],yend=seg$y1[i],line=list(width=min(8,1+seg$weight[i]),color="rgba(101,20,47,.25)"),hoverinfo="text",text=paste0(seg[[left_col]][i]," ↔ ",seg[[right_col]][i],"<br>Records: ",seg$weight[i]),showlegend=FALSE);sizes<-bind_rows(edges|>group_by(label=.data[[left_col]])|>summarise(n=sum(weight),.groups="drop")|>mutate(group=left_label),edges|>group_by(label=.data[[right_col]])|>summarise(n=sum(weight),.groups="drop")|>mutate(group=right_label));nodes<-nodes|>left_join(sizes,by=c("label","group"));p|>add_markers(data=nodes,x=~x,y=~y,size=~n,sizes=c(12,34),marker=list(color="#B92F53"),text=~paste0("<b>",label,"</b><br>",group,"<br>Linked records: ",n),hoverinfo="text",showlegend=FALSE)|>add_text(data=nodes,x=~x,y=~y,text=~label,textposition=~ifelse(x==0,"middle left","middle right"),hoverinfo="skip",showlegend=FALSE)|>layout(xaxis=list(visible=FALSE,range=c(-.45,1.45)),yaxis=list(visible=FALSE),margin=list(l=150,r=150,t=30,b=30))}
  output$explore_scientific_network<-renderPlotly({
    d <- explore_filtered() |>
      mutate(
        technology=coalesce(omics_modality,"Technology not reported"),
        sample_type=sample_label(biospecimen_class),
        disease_question=coalesce(condition,clinical_context,population_category,"Not reported"),
        domain=coalesce(research_domain,"Not reported")
      )
    switch(input$explore_network_view,
      technology_disease=make_bipartite_plot(d,"technology","disease_question","Technology","Disease / question"),
      sample_technology=make_bipartite_plot(d,"sample_type","technology","Sample type","Technology"),
      technology_domain=make_bipartite_plot(d,"technology","domain","Technology","Research domain"))
  })
  output$map_coverage<-renderUI({
    d<-community_evidence(); mapped<-d|>filter(!is.na(latitude),!is.na(longitude))
    companies<-mfox$companies |> filter(!is.na(latitude),!is.na(longitude))
    div(class="map-status map-status-ecosystem",
      strong(paste0("Overall mapped coverage · ",n_distinct(mapped$institution)," research institutions")),
      span(paste0(nrow(companies)," companies · ",n_distinct(mapped$person_id)," publication-linked researchers; filters update the markers below")),
      div(class="map-inline-legend",span(class="map-dot map-dot-research"),"Research",span(class="map-dot map-dot-company"),"Companies"))
  })
  output$people_map<-renderLeaflet({
    d<-community_evidence()
    if(length(input$map_technology))d<-d|>filter(technology%in%input$map_technology)
    if(length(input$map_sample))d<-d|>filter(sample_type%in%input$map_sample)
    if(length(input$map_disease))d<-d|>filter(disease_question%in%input$map_disease)
    d<-d|>filter(!is.na(latitude),!is.na(longitude),!is.na(institution))
    labs<-d|>group_by(institution,city,country,latitude,longitude)|>summarise(
      researchers=paste(sort(unique(link_or_text(name,public_profile_url))),collapse="<br>"),
      technologies=paste(sort(unique(technology)),collapse=" · "),
      samples=paste(sort(unique(sample_label(sample_type))),collapse=" · "),
      topics=paste(sort(unique(disease_question)),collapse=" · "),
      papers=paste(head(unique(link_or_text(paste0(year," · ",title),source_url)),8),collapse="<br>"),
      n_researchers=n_distinct(person_id),n_publications=n_distinct(publication_id),.groups="drop")

    companies <- mfox$companies |> filter(!is.na(latitude),!is.na(longitude))
    if(length(input$map_technology)){
      tech_terms <- unique(unlist(lapply(input$map_technology,plan_technology_terms)))
      companies <- companies[company_text_has_any(paste(companies$technology,companies$utilization_domain),tech_terms),,drop=FALSE]
    }
    if(length(input$map_sample)){
      sample_terms <- unique(unlist(lapply(input$map_sample,plan_sample_terms)))
      companies <- companies[company_text_has_any(paste(companies$sample_type,companies$material_exploited,companies$sample_collection),sample_terms),,drop=FALSE]
    }
    if(length(input$map_disease)){
      condition_terms <- unique(unlist(lapply(input$map_disease,plan_condition_terms)))
      companies <- companies[company_text_has_any(paste(companies$disease_application,companies$utilization_domain),condition_terms),,drop=FALSE]
    }

    m<-leaflet()|>addTiles(urlTemplate="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",attribution="&copy; OpenStreetMap contributors")
    if(nrow(labs)){
      m<-m|>addCircleMarkers(data=labs,lng=~longitude,lat=~latitude,radius=~pmin(15,6+n_researchers),color="#65142F",fillColor="#B92F53",fillOpacity=.82,
        label=~institution,popup=~paste0("<div class='map-popup'><div class='map-popup-type'>Research institution</div><strong>",institution,"</strong><br>",city,", ",country,"<br><small>",n_researchers," researchers · ",n_publications," publications</small><hr><b>Researchers</b><br>",researchers,"<br><br><b>Technologies</b><br>",technologies,"<br><br><b>Samples</b><br>",samples,"<br><br><b>Disease / question</b><br>",topics,"<br><br><b>Publications</b><br>",papers,"</div>"))
    }
    if(nrow(companies)){
      m<-m|>addCircleMarkers(data=companies,lng=~longitude,lat=~latitude,radius=8,color="#205F64",fillColor="#2F7E83",fillOpacity=.88,weight=2,
        label=~company,popup=~paste0("<div class='map-popup'><div class='map-popup-type company-type'>Company / innovator</div><strong>",company,"</strong><br>",city,", ",country,"<hr><b>Utilization</b><br>",utilization_domain,"<br><br><b>Technology</b><br>",technology,"<br><br><b>Application</b><br>",disease_application,"<br><br><a href='",website,"' target='_blank' rel='noopener'>Website ↗</a></div>"))
    }
    m <- m |> addLegend(position="bottomright",colors=c("#B92F53","#2F7E83"),labels=c("Research institutions","Companies / innovators"),opacity=.9,title="MFOX community")
    coords <- bind_rows(
      if(nrow(labs)) labs |> transmute(longitude,latitude) else tibble::tibble(longitude=numeric(),latitude=numeric()),
      if(nrow(companies)) companies |> transmute(longitude,latitude) else tibble::tibble(longitude=numeric(),latitude=numeric())
    )
    if(nrow(coords)>1)m<-m|>fitBounds(min(coords$longitude),min(coords$latitude),max(coords$longitude),max(coords$latitude))
    m
  })
  output$community_pub_table<-renderDT({
    db_tick()
    curated <- mfox$community_publications |>
      mutate(Paper=link_or_text(ifelse(is.na(year),title,paste0(year," · ",title)),source_url),
             Identifier=coalesce(as.character(doi),as.character(pmid),""),Status="Curated MFOX publication") |>
      transmute(Paper,Identifier,Journal=journal,Type=publication_type,`MF scope`=mf_scope,Status)
    acc <- accepted_publications()
    if(nrow(acc)){
      acc <- acc |> mutate(Paper=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),
                          Identifier=coalesce(identifier_or_url,""),Journal="",Type="Community-submitted publication",
                          `MF scope`="Accepted contribution · structured evidence curation pending",Status="Accepted in MFOX Den") |>
        select(Paper,Identifier,Journal,Type,`MF scope`,Status)
      curated <- bind_rows(curated,acc) |>
        distinct(Paper,.keep_all=TRUE)
    }
    datatable(curated,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })

  join_message <- reactiveVal(NULL)
  join_required_ok <- reactive({
    nzchar(trimws(input$join_name %||% "")) &&
      nzchar(trimws(input$join_institution %||% "")) &&
      nzchar(trimws(input$join_country %||% "")) &&
      (nzchar(trimws(input$join_website %||% "")) || nzchar(trimws(input$join_profile %||% "")) || nzchar(trimws(input$join_verification %||% "")))
  })
  output$join_status<-renderUI({
    msg <- join_message()
    if(!is.null(msg)) return(div(class=if(isTRUE(msg$ok))"submission-ready" else "empty-note",strong(msg$title),p(msg$text)))
    input$prepare_join
    if(input$prepare_join<1) return(NULL)
    if(!join_required_ok()) return(div(class="empty-note","Please provide a name, institution/organization, country, and at least one public website/profile/verification link."))
    company_profile <- identical(input$join_profile_type,"Company / innovator") || identical(input$join_sector,"Company / industry")
    div(class="submission-ready",
      strong("Structured profile ready to submit."),
      p("Curators can map these fields directly into the relevant MFOX directory after verification."),
      tags$ul(
        tags$li(strong("Profile: "),input$join_profile_type," · ",input$join_sector),
        tags$li(strong("Name: "),input$join_name),
        tags$li(strong("Organization: "),input$join_institution),
        tags$li(strong("Location: "),paste(Filter(nzchar,c(input$join_city,input$join_region,input$join_country)),collapse=", ")),
        tags$li(strong("Website/profile: "),ifelse(nzchar(input$join_website),input$join_website,input$join_profile)),
        if(company_profile) tags$li(strong("Company maturity: "),input$join_company_maturity)
      ))
  })

  observeEvent(input$submit_join,{
    join_message(NULL)
    if(!join_required_ok()){
      join_message(list(ok=FALSE,title="More information needed",text="Please provide a name, institution/organization, country, and at least one public website/profile/verification link.")); return()
    }
    path <- file.path("data","community_submissions.csv")
    company_profile <- identical(input$join_profile_type,"Company / innovator") || identical(input$join_sector,"Company / industry")
    row <- tibble::tibble(
      submission_id=paste0("COMM-",format(Sys.time(),"%Y%m%d-%H%M%S"),"-",sprintf("%03d",sample.int(999,1))),
      submitted_date=as.character(Sys.Date()),
      profile_type=as.character(input$join_profile_type %||% ""), sector=as.character(input$join_sector %||% ""),
      name_or_lab=trimws(input$join_name %||% ""), institution=trimws(input$join_institution %||% ""), role=trimws(input$join_role %||% ""),
      city=trimws(input$join_city %||% ""), region=trimws(input$join_region %||% ""), country=trimws(input$join_country %||% ""),
      website=trimws(input$join_website %||% ""), profile_url=trimws(input$join_profile %||% ""), orcid=trimws(input$join_orcid %||% ""),
      research_interests=trimws(input$join_interests %||% ""), expertise_tags=trimws(input$join_expertise %||% ""),
      clinical_context=paste(input$join_context %||% character(),collapse="; "), sample_types=paste(input$join_samples %||% character(),collapse="; "),
      technologies=paste(input$join_technologies %||% character(),collapse="; "), collaboration_interests=trimws(input$join_collaboration %||% ""),
      company_utilization_domain=if(company_profile)paste(input$join_company_use %||% character(),collapse=";") else "",
      company_material_exploited=if(company_profile)paste(input$join_company_material %||% character(),collapse=";") else "",
      company_technology_product=if(company_profile)trimws(input$join_company_technology %||% "") else "",
      company_disease_application=if(company_profile)trimws(input$join_company_application %||% "") else "",
      company_development_stage=if(company_profile)as.character(input$join_company_maturity %||% "") else "",
      company_evidence_status=if(company_profile)as.character(input$join_company_evidence %||% "") else "",
      verification_url=trimws(input$join_verification %||% ""), status="Submitted", curator_notes=""
    )
    ans <- tryCatch({
      old <- if(file.exists(path)) readr::read_csv(path,show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character()),na=c("","NA")) else tibble::tibble()
      for(nm in union(names(old),names(row))){
        if(!nm %in% names(old)) old[[nm]] <- NA_character_
        if(!nm %in% names(row)) row[[nm]] <- NA_character_
        old[[nm]] <- as.character(old[[nm]]); row[[nm]] <- as.character(row[[nm]])
      }
      old <- old[,names(row),drop=FALSE]
      readr::write_csv(dplyr::bind_rows(old,row),path)
      list(ok=TRUE,title="Submitted for curator review",text="Your structured profile was saved to the MFOX community review queue. Nothing is published automatically.")
    },error=function(e) list(ok=FALSE,title="Submission could not be saved",text=paste("The app could not write to its community review file:",conditionMessage(e))))
    join_message(ans)
  },ignoreInit=TRUE)



  company_filtered <- reactive({
    db_tick()
    d <- mfox$companies
    q <- trimws(input$company_search %||% "")
    if(nzchar(q)){
      hay <- paste(d$company,d$technology,d$disease_application,d$utilization_domain,d$material_exploited,d$country,d$city,sep=" | ")
      d <- d[grepl(q,hay,ignore.case=TRUE),,drop=FALSE]
    }
    if(length(input$company_use)) d <- d[company_term_match(d$utilization_domain,input$company_use),,drop=FALSE]
    if(length(input$company_material)) d <- d[company_term_match(d$material_exploited,input$company_material),,drop=FALSE]
    if(length(input$company_maturity)) d <- d |> filter(maturity_level %in% input$company_maturity)
    if(length(input$company_evidence)) d <- d |> filter(evidence_status %in% input$company_evidence)
    d
  })

  observeEvent(input$clear_company_filters,{
    updateTextInput(session,"company_search",value="")
    updateSelectizeInput(session,"company_use",selected=character())
    updateSelectizeInput(session,"company_material",selected=character())
    updateSelectizeInput(session,"company_maturity",selected=character())
    updateSelectizeInput(session,"company_evidence",selected=character())
  },ignoreInit=TRUE)

  output$company_metrics<-renderUI({
    d<-company_filtered()
    pending <- accepted_companies()
    uses <- split_company_terms(d$utilization_domain)
    mats <- split_company_terms(d$material_exploited)
    div(class="metric-strip company-metric-strip",
      div(strong(nrow(d)),span(if(nrow(d)==nrow(mfox$companies)) "curated companies" else "matching companies")),
      div(strong(length(uses)),span("utilization areas")),
      div(strong(length(mats)),span("material classes")),
      div(strong(n_distinct(d$country[!is.na(d$country)&d$country!=""])),span("countries")),
      div(strong(nrow(pending)),span("accepted · curation pending"))
    )
  })

  company_icon<-function(label){
    z<-tolower(label)
    nm<-if(str_detect(z,"rna|pcr|sequenc|genom|dna")) "dna" else if(str_detect(z,"protein|biomarker|hormone")) "vial" else if(str_detect(z,"metabol|lipid")) "flask" else if(str_detect(z,"microbi")) "bacteria" else if(str_detect(z,"ev|vesicle")) "circle-nodes" else if(str_detect(z,"cell|regener")) "microscope" else if(str_detect(z,"collect|preanalytic")) "droplet" else if(str_detect(z,"screen|diagnos")) "stethoscope" else if(str_detect(z,"monitor")) "chart-line" else "flask-vial"
    icon(nm)
  }

  output$company_utilization_map<-renderUI({
    d<-company_filtered()
    if(!nrow(d)) return(div(class="empty-note","No curated companies match the current discovery filters."))
    z <- d |> select(company,website,utilization_domain) |> tidyr::separate_rows(utilization_domain,sep=";") |>
      mutate(utilization_domain=trimws(utilization_domain)) |> filter(!is.na(utilization_domain),utilization_domain!="") |>
      group_by(utilization_domain) |> summarise(n=n_distinct(company),Companies=paste(sort(unique(company)),collapse=" · "),.groups="drop") |>
      arrange(desc(n),utilization_domain)
    div(class="company-use-grid",lapply(seq_len(nrow(z)),function(i){
      div(class="company-use-card",
        div(class="company-use-card-icon",company_icon(z$utilization_domain[i])),
        div(class="company-use-card-copy",
          div(class="company-use-card-head",h3(z$utilization_domain[i]),span(class="company-use-count",z$n[i])),
          p(z$Companies[i])
        )
      )
    }))
  })

  output$company_utilization_flow <- renderPlotly({
    d <- company_filtered()
    validate(need(nrow(d)>0,"No curated companies match the current discovery filters."))
    mat_links <- d |> select(company,material_exploited) |>
      tidyr::separate_rows(material_exploited,sep=";") |>
      mutate(material=trimws(material_exploited)) |> filter(!is.na(material),material!="") |>
      distinct(material,company) |>
      transmute(source=paste0("material:",material),target=paste0("company:",company),value=1L,
                detail=paste0(material," → ",company))
    use_links <- d |> select(company,utilization_domain) |>
      tidyr::separate_rows(utilization_domain,sep=";") |>
      mutate(utilization=trimws(utilization_domain)) |> filter(!is.na(utilization),utilization!="") |>
      distinct(company,utilization) |>
      transmute(source=paste0("company:",company),target=paste0("use:",utilization),value=1L,
                detail=paste0(company," → ",utilization))
    links <- bind_rows(mat_links,use_links)
    validate(need(nrow(links)>0,"No utilization relationships are available for the current filters."))
    nodes <- tibble::tibble(id=unique(c(links$source,links$target))) |>
      mutate(label=sub("^[^:]+:","",id),kind=sub(":.*$","",id),
             color=dplyr::recode(kind,material="#E98567",company="#7A1832",use="#2F7E83",.default="#8797A5"))
    idx <- setNames(seq_len(nrow(nodes))-1,nodes$id)
    plot_ly(type="sankey",orientation="h",arrangement="snap",
      node=list(label=nodes$label,color=nodes$color,pad=16,thickness=18,line=list(color="rgba(80,30,45,.12)",width=1),
                hovertemplate="<b>%{label}</b><extra></extra>"),
      link=list(source=unname(idx[links$source]),target=unname(idx[links$target]),value=links$value,
                customdata=links$detail,color="rgba(143,29,57,.14)",hovertemplate="%{customdata}<extra></extra>")) |>
      layout(margin=list(l=10,r=10,t=8,b=8),font=list(size=12,color="#4f3540"))
  })

  output$company_material_use_heatmap <- renderPlotly({
    d <- company_filtered()
    validate(need(nrow(d)>0,"No curated companies match the current discovery filters."))
    z <- d |> select(company,material_exploited,utilization_domain) |>
      tidyr::separate_rows(material_exploited,sep=";") |> mutate(Material=trimws(material_exploited)) |>
      tidyr::separate_rows(utilization_domain,sep=";") |> mutate(Utilization=trimws(utilization_domain)) |>
      filter(Material!="",Utilization!="") |> distinct(company,Material,Utilization) |>
      count(Material,Utilization,name="Companies")
    validate(need(nrow(z)>0,"No material/utilization relationships are available for the current filters."))
    mats <- rev(sort(unique(z$Material))); uses <- sort(unique(z$Utilization))
    grid <- tidyr::expand_grid(Material=mats,Utilization=uses) |> left_join(z,by=c("Material","Utilization")) |> mutate(Companies=coalesce(Companies,0L))
    mat <- matrix(grid$Companies,nrow=length(mats),ncol=length(uses),byrow=TRUE,dimnames=list(mats,uses))
    plot_ly(x=uses,y=mats,z=mat,type="heatmap",colorscale=list(list(0,"#FFF8F6"),list(1,"#8F1D39")),showscale=TRUE,
            hovertemplate="<b>%{y}</b><br>%{x}<br>Companies listing both: %{z}<extra></extra>") |>
      layout(xaxis=list(title="Utilization area",tickangle=-28),yaxis=list(title="Menstrual-fluid material"),margin=list(l=170,r=30,t=15,b=150))
  })

  output$company_material_use_table<-renderDT({
    d<-company_filtered()
    if(!nrow(d)) return(datatable(tibble::tibble(Status="No companies match the current filters."),rownames=FALSE,options=list(dom="t")))
    z<-d |> select(company,material_exploited,utilization_domain) |>
      tidyr::separate_rows(material_exploited,sep=";") |> mutate(Material=trimws(material_exploited)) |> select(-material_exploited) |>
      tidyr::separate_rows(utilization_domain,sep=";") |> mutate(Utilization=trimws(utilization_domain)) |>
      filter(Material!="",Utilization!="") |> group_by(Material,Utilization) |>
      summarise(Companies=paste(sort(unique(company)),collapse=", "),Count=n_distinct(company),.groups="drop") |>
      arrange(Material,desc(Count),Utilization)
    datatable(z,rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })

  output$company_overview_cards<-renderUI({
    d<-company_filtered()
    curated_cards <- if(nrow(d)) lapply(seq_len(nrow(d)),function(i){
      techs<-trimws(str_split(d$technology[i],";")[[1]]);techs<-techs[nzchar(techs)]
      uses<-trimws(str_split(d$utilization_domain[i],";")[[1]]);uses<-uses[nzchar(uses)]
      mats<-trimws(str_split(d$material_exploited[i],";")[[1]]);mats<-mats[nzchar(mats)]
      div(class="company-overview-card company-discovery-card",
        div(class="company-card-head",
          div(class="entity-heading",entity_mark(d$company[i]),div(
            div(class="company-title-row",h3(d$company[i]),div(class="company-card-status-inline",span(class="company-status-label","Maturity"),span(class="company-maturity-badge",icon("signal"),d$maturity_level[i]))),
            div(class="company-location",icon("location-dot"),paste(na.omit(c(d$city[i],d$country[i])),collapse=", ")))),
          if(!is.na(d$website[i])&&nzchar(d$website[i])) tags$a(href=d$website[i],target="_blank",class="company-site",icon("arrow-up-right-from-square")," Website")
        ),
        div(class="company-use-chips",lapply(uses,function(tt) span(class="company-use-chip",company_icon(tt),tt))),
        if(!is.na(d$disease_application[i])&&nzchar(d$disease_application[i])) div(class="company-card-section",strong("Application"),p(d$disease_application[i])),
        div(class="company-card-section",strong("Biological material"),div(class="company-material-list",lapply(mats,function(mm) span(mm)))),
        div(class="company-card-section",strong("Technology / product"),div(class="company-tech-icons",lapply(techs,function(tt) div(class="company-tech-chip",company_icon(tt),span(tt))))),
        div(class="company-evidence-row",
          span(class="company-status-label","Evidence"),
          span(class="company-evidence-badge",icon("shield-halved"),d$evidence_status[i])
        ),
      )
    }) else list()
    filters_active <- nzchar(trimws(input$company_search %||% "")) || length(input$company_use) || length(input$company_material) || length(input$company_maturity) || length(input$company_evidence)
    pending <- accepted_companies()
    pending_cards <- if(!filters_active && nrow(pending)) lapply(seq_len(nrow(pending)),function(i){
      r<-pending[i,];ident<-as.character(r$identifier_or_url %||% "")
      div(class="company-overview-card company-pending-card",
        div(class="company-card-head",
          div(h3(ifelse(is.na(r$title_or_name)||r$title_or_name=="","Untitled company",r$title_or_name)),
              div(class="company-location",icon("clock"),"Accepted submission")),
          if(grepl("^https?://",ident,ignore.case=TRUE)) tags$a(href=ident,target="_blank",class="company-site",icon("arrow-up-right-from-square")," Source")
        ),
        p(class="muted",ifelse(is.na(r$mf_use_description)||r$mf_use_description=="","Company details not yet structured.",r$mf_use_description)),
        div(class="resource-status resource-status-pending",icon("hourglass-half")," Accepted by curator · company curation pending")
      )
    }) else list()
    if(!length(curated_cards) && !length(pending_cards)) return(div(class="empty-note","No companies match the current discovery filters."))
    div(class="company-overview-grid",c(curated_cards,pending_cards))
  })

  output$company_evidence_summary<-renderUI({
    d<-company_filtered()
    if(!nrow(d)) return(NULL)
    stages <- d |> mutate(label=coalesce(as.character(maturity_level),"Not categorized")) |> count(label,sort=TRUE)
    evidence <- d |> mutate(label=coalesce(as.character(evidence_status),"Not categorized")) |> count(label,sort=TRUE)
    chip_row <- function(x) div(class="status-chip-row",lapply(seq_len(nrow(x)),function(i) span(class="status-count-chip",strong(x$n[i]),x$label[i])))
    div(class="company-evidence-summary",
      div(class="status-summary-block",h4(icon("signal")," Development stage"),p("Where the product, platform or service sits in development."),chip_row(stages)),
      div(class="status-summary-block",h4(icon("file-lines")," Evidence status"),p("The strongest public evidence type currently curated by MFOX."),chip_row(evidence))
    )
  })

  output$plan_companies<-renderUI({
    if(!plan_definition_active()) return(div(class="empty-note","Define at least one study feature and build study intelligence to see companies with translational overlap."))
    q <- plan_query()
    d <- company_plan_overlap(mfox$companies,q)
    requested <- c(
      if(q$condition!="Any") paste0("Context: ",q$condition),
      if(q$omics!="Any") paste0("Technology: ",q$omics),
      if(q$biospecimen!="Any") paste0("Sample: ",sample_label(q$biospecimen)),
      if(q$longitudinal!="Any") paste0("Design: longitudinal = ",q$longitudinal)
    )
    if(!nrow(d)) return(div(class="empty-note",
      h4("No company overlap is curated for this study definition yet"),
      p("MFOX looked for overlap with the clinical context, technology family, sample/material and longitudinal use represented in your plan."),
      if(length(requested)) p(class="muted",paste(requested,collapse=" · "))))
    cards <- lapply(seq_len(nrow(d)),function(i){
      r <- d[i,]
      chips <- tagList(
        if(isTRUE(r$match_condition)) span(class="company-overlap-chip",icon("stethoscope"),paste0("Context overlap · ",plan_condition_overlap_label(q$condition))),
        if(isTRUE(r$match_technology)) span(class="company-overlap-chip",icon("microscope"),paste0("Technology family · ",plan_technology_family_label(q$omics))),
        if(isTRUE(r$match_sample)) span(class="company-overlap-chip",icon("droplet"),paste0("Sample · ",sample_label(q$biospecimen))),
        if(isTRUE(r$match_longitudinal)) span(class="company-overlap-chip",icon("repeat"),"Longitudinal monitoring")
      )
      overlap_label <- paste(c(
        if(isTRUE(r$match_condition)) "clinical context",
        if(isTRUE(r$match_technology)) "technology family",
        if(isTRUE(r$match_sample)) "sample/material",
        if(isTRUE(r$match_longitudinal)) "longitudinal use"
      ),collapse=" + ")
      div(class="company-mini-card company-overlap-card",
        div(class="company-overlap-head",
          h4(tags$a(href=r$website,target="_blank",rel="noopener",r$company," ↗")),
          span(class="company-overlap-count",paste0(r$overlap_count," overlap",ifelse(r$overlap_count==1,"","s")))),
        if(length(chips)>0) div(class="company-overlap-chips",chips),
        p(class="company-overlap-why",strong("Why it overlaps: "),overlap_label," with your proposed study."),
        p(strong("Company use: "),r$disease_application),
        p(strong("Technology / product: "),r$technology),
        p(strong("Menstrual-fluid material: "),r$material_exploited),
        div(class="company-overlap-foot",span(r$maturity_level),span(r$evidence_status))
      )
    })
    tagList(
      div(class="company-plan-intro",
        h3("Companies with overlap to your study"),
        p("These suggestions are based on shared disease/context, technology family, menstrual-fluid material or longitudinal use. They indicate translational overlap, not endorsement or equivalent scientific validation."),
        if(length(requested)) div(class="company-plan-query",strong("Your plan: "),paste(requested,collapse=" · "))),
      div(class="company-card-grid",cards)
    )
  })

  contrib_message <- reactiveVal(NULL)
  contrib_ok <- reactiveVal(FALSE)
  observeEvent(input$submit_contribution,{
    title <- trimws(input$contrib_title %||% "")
    ident <- trimws(input$contrib_identifier %||% "")
    if(!nzchar(title) && !nzchar(ident)){
      contrib_ok(FALSE)
      contrib_message("Please enter at least a title/name or a DOI, PMID, accession or URL. The menstrual-fluid description is helpful but is not required to submit a paper.")
      return()
    }
    path <- file.path("data","contributions.csv")
    old <- if(file.exists(path)) readr::read_csv(path,show_col_types=FALSE,na=c("","NA")) else tibble::tibble()
    sid <- paste0("SUB",format(Sys.time(),"%Y%m%d%H%M%S"),sprintf("%03d",as.integer((as.numeric(Sys.time())*1000)%%1000)))
    if(isTRUE(input$contrib_notify) && !grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", trimws(input$contrib_email %||% ""))){ contrib_ok(FALSE); contrib_message("Please enter a valid email address if you want decision notifications."); return() }
    row <- tibble::tibble(submission_id=sid,submitted_date=as.character(Sys.Date()),submission_type=input$contrib_type,title_or_name=title,identifier_or_url=ident,sample_term_used=input$contrib_term,mf_use_description=trimws(input$contrib_mf_use %||% ""),associated_publication=trimws(input$contrib_assoc %||% ""),submitter_name=trimws(input$contrib_name %||% ""),submitter_contact=trimws(input$contrib_contact %||% ""),submitter_email=trimws(input$contrib_email %||% ""),notification_opt_in=isTRUE(input$contrib_notify),notification_status="Not requested",notified_at=NA_character_,notes=trimws(input$contrib_notes %||% ""),status="Submitted",curator_notes="") |> mutate(notification_status=ifelse(notification_opt_in,"Pending decision","Not requested"))
    tryCatch({
      # Normalize persisted submission schema before appending. Older CSV releases
      # stored notification_opt_in as text, while checkboxInput returns logical.
      if(!"notification_opt_in" %in% names(old)) old$notification_opt_in <- FALSE
      old$notification_opt_in <- dplyr::case_when(
        is.na(old$notification_opt_in) ~ FALSE,
        tolower(trimws(as.character(old$notification_opt_in))) %in% c("true","t","1","yes","y") ~ TRUE,
        TRUE ~ FALSE
      )
      row$notification_opt_in <- as.logical(row$notification_opt_in)
      # Keep every non-boolean submission field character-typed across old/new releases.
      char_cols <- setdiff(union(names(old),names(row)),"notification_opt_in")
      for(nm in char_cols){
        if(!nm %in% names(old)) old[[nm]] <- NA_character_
        if(!nm %in% names(row)) row[[nm]] <- NA_character_
        old[[nm]] <- as.character(old[[nm]])
        row[[nm]] <- as.character(row[[nm]])
      }
      for(nm in c("submitter_email","notification_status","notified_at")){
        if(!nm %in% names(old)) old[[nm]] <- NA_character_
        old[[nm]] <- as.character(old[[nm]])
        row[[nm]] <- as.character(row[[nm]])
      }
      readr::write_csv(dplyr::bind_rows(old,row),path)
      contrib_ok(TRUE)
      contrib_message(paste0("Submitted successfully — ",sid," is now in the curator queue."))
    },error=function(e){
      contrib_ok(FALSE)
      contrib_message(paste0("Submission could not be saved on this deployment. Error: ",conditionMessage(e)))
    })
  },ignoreInit=TRUE)
  output$contrib_status <- renderUI({
    req(contrib_message())
    cls <- if(isTRUE(contrib_ok())) "submission-ready" else "submission-error"
    ico <- if(isTRUE(contrib_ok())) icon("circle-check") else icon("triangle-exclamation")
    div(class=cls,ico,span(contrib_message()))
  })

  fox_authenticated <- reactiveVal(FALSE)
  fox_message <- reactiveVal(NULL)
  contributions_live <- reactiveVal({
    path <- file.path("data","contributions.csv")
    if(file.exists(path)) readr::read_csv(path,show_col_types=FALSE,na=c("","NA")) else tibble::tibble()
  })
  accepted_contributions <- reactive({
    d<-contributions_live()
    if(!nrow(d)||!"status"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(startsWith(as.character(status),"Accepted"))
  })
  accepted_pending <- reactive({
    d<-accepted_contributions()
    if(!nrow(d)||!"status"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(!grepl("curated into database",as.character(status),ignore.case=TRUE))
  })
  accepted_publications <- reactive({
    d<-accepted_pending()
    if(!nrow(d)||!"submission_type"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(grepl("Publication",as.character(submission_type),ignore.case=TRUE))
  })
  accepted_datasets <- reactive({
    d<-accepted_pending()
    if(!nrow(d)||!"submission_type"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(grepl("Dataset",as.character(submission_type),ignore.case=TRUE))
  })
  accepted_projects <- reactive({
    d<-accepted_pending()
    if(!nrow(d)||!"submission_type"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(grepl("Project|consortium|program",as.character(submission_type),ignore.case=TRUE))
  })
  accepted_companies <- reactive({
    d<-accepted_pending()
    if(!nrow(d)||!"submission_type"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(grepl("Company",as.character(submission_type),ignore.case=TRUE))
  })
  accepted_people_labs <- reactive({
    d<-accepted_pending()
    if(!nrow(d)||!"submission_type"%in%names(d)) return(d[0,,drop=FALSE])
    d |> filter(grepl("Researcher|lab",as.character(submission_type),ignore.case=TRUE))
  })
  accepted_resource_link <- function(title,identifier){
    title<-ifelse(is.na(title)|!nzchar(trimws(title)),"Untitled accepted contribution",as.character(title))
    identifier<-ifelse(is.na(identifier),"",trimws(as.character(identifier)))
    url<-""
    if(grepl("^https?://",identifier,ignore.case=TRUE)) url<-identifier
    else if(grepl("^10\\.",identifier)) url<-paste0("https://doi.org/",identifier)
    else if(grepl("^[0-9]{6,9}$",identifier)) url<-paste0("https://pubmed.ncbi.nlm.nih.gov/",identifier,"/")
    safe_title<-htmltools::htmlEscape(title)
    if(nzchar(url)) paste0('<a href="',htmltools::htmlEscape(url),'" target="_blank" rel="noopener">',safe_title,' ↗</a>') else safe_title
  }
  candidate_votes <- reactiveVal({
    vp <- file.path("data","candidate_votes.csv")
    # Explicit character types are essential here. An empty CSV (header only)
    # is otherwise inferred by readr as logical(0), which breaks the first
    # bind_rows() when a real character vote ("for" / "against") arrives.
    v <- if(file.exists(vp)) {
      readr::read_csv(
        vp,
        show_col_types=FALSE,
        col_types=readr::cols(
          candidate_id=readr::col_character(),
          session_id=readr::col_character(),
          vote=readr::col_character(),
          voted_at=readr::col_character()
        )
      )
    } else {
      tibble::tibble(candidate_id=character(),session_id=character(),vote=character(),voted_at=character())
    }
    if(!"candidate_id"%in%names(v))v$candidate_id<-rep(NA_character_,nrow(v))
    if(!"session_id"%in%names(v))v$session_id<-rep(NA_character_,nrow(v))
    if(!"vote"%in%names(v))v$vote<-rep(NA_character_,nrow(v))
    if(!"voted_at"%in%names(v))v$voted_at<-rep(NA_character_,nrow(v))
    v$candidate_id <- as.character(v$candidate_id)
    v$session_id <- as.character(v$session_id)
    v$vote <- as.character(v$vote)
    v$voted_at <- as.character(v$voted_at)
    if(nrow(v)) {
      is_against <- tolower(v$vote) %in% c("against","down","-1")
      v$vote <- ifelse(is_against,"against","for")
    } else {
      v$vote <- character(0)
    }
    v
  })

  # Deliberately do not depend on session$token here. A private random ID is
  # generated once for this Shiny session and is used only to de-duplicate a
  # user's vote within the current session.
  vote_session_id <- paste0(
    "session-",format(Sys.time(),"%Y%m%d%H%M%S"),"-",
    paste(sample(c(letters,LETTERS,0:9),20,replace=TRUE),collapse="")
  )

  vote_counts <- reactive({
    v<-candidate_votes()
    if(!nrow(v)) return(tibble::tibble(candidate_id=character(),votes_for=integer(),votes_against=integer(),balance=integer(),total_votes=integer()))
    v |> group_by(candidate_id) |> summarise(votes_for=sum(vote=="for"),votes_against=sum(vote=="against"),balance=votes_for-votes_against,total_votes=n(),.groups="drop")
  })

  candidate_vote_totals <- function(v){
    list(
      voters=dplyr::n_distinct(v$session_id),
      total_for=sum(v$vote=="for",na.rm=TRUE),
      total_against=sum(v$vote=="against",na.rm=TRUE),
      total_net=sum(v$vote=="for",na.rm=TRUE)-sum(v$vote=="against",na.rm=TRUE)
    )
  }

  candidate_vote_row <- function(v,candidate_id){
    z<-v |> filter(.data$candidate_id==.env$candidate_id)
    vf<-sum(z$vote=="for",na.rm=TRUE)
    va<-sum(z$vote=="against",na.rm=TRUE)
    list(votes_for=vf,votes_against=va,balance=vf-va)
  }

  cast_candidate_vote <- function(candidate_id,direction){
    if(!nzchar(candidate_id) || !candidate_id %in% as.character(mfox$candidates$candidate_id)) stop("Unknown candidate.")
    if(!direction %in% c("for","against")) stop("Unknown vote direction.")
    v<-candidate_votes()
    # Defensive typing before append/update. This protects voting even if an
    # older deployment wrote an empty or oddly inferred CSV schema.
    v$candidate_id <- as.character(v$candidate_id)
    v$session_id <- as.character(v$session_id)
    v$vote <- as.character(v$vote)
    v$voted_at <- as.character(v$voted_at)
    idx<-which(v$candidate_id==candidate_id & v$session_id==vote_session_id)
    now<-format(Sys.time(),"%Y-%m-%d %H:%M:%S")
    if(length(idx)){
      v$vote[idx[1]]<-direction
      v$voted_at[idx[1]]<-now
      if(length(idx)>1)v<-v[-idx[-1],,drop=FALSE]
    } else {
      v<-bind_rows(v,tibble::tibble(candidate_id=candidate_id,session_id=vote_session_id,vote=direction,voted_at=now))
    }

    # Update the in-session store first. Disk persistence is a separate best-
    # effort step so a read-only deployment cannot make the button appear dead.
    candidate_votes(v)
    persisted<-tryCatch({
      readr::write_csv(v,file.path("data","candidate_votes.csv"))
      TRUE
    },error=function(e) FALSE)

    row<-candidate_vote_row(v,candidate_id)
    totals<-candidate_vote_totals(v)
    c(list(ok=TRUE,persisted=persisted,direction=direction),row,totals)
  }

  # This summary is rendered once. Vote acknowledgements update the four
  # numeric spans directly in the browser, avoiding Shiny's recalculating UI.
  output$candidate_vote_summary <- renderUI({
    v<-isolate(candidate_votes()); totals<-candidate_vote_totals(v); c<-mfox$candidates
    div(class="candidate-kpis candidate-kpis-voting",
      div(strong(nrow(c)),span("candidates")),
      div(strong(id="vote-summary-voters",totals$voters),span("community voters")),
      div(class="kpi-for",strong(id="vote-summary-for",totals$total_for),span("votes for")),
      div(class="kpi-against",strong(id="vote-summary-against",totals$total_against),span("votes against")),
      div(strong(id="vote-summary-net",totals$total_net),span("net balance"))
    )
  })

  output$fox_candidate_vote_summary <- renderUI({
    v<-candidate_votes(); vc<-vote_counts(); totals<-candidate_vote_totals(v)
    div(class="fox-vote-summary",span(icon("users"),paste(totals$voters,"voters")),span(class="vote-for-text",icon("thumbs-up"),paste(totals$total_for,"for")),span(class="vote-against-text",icon("thumbs-down"),paste(totals$total_against,"against")),span(paste(sum(vc$balance),"net")))
  })

  # Candidate cards are also rendered once. They contain plain HTML buttons,
  # not Shiny actionButtons, and no reactive output lives inside a card.
  output$candidate_cards <- renderUI({
    c<-mfox$candidates
    if(!nrow(c)) return(div(class="empty-note","No automated candidates are waiting for review."))
    v0<-isolate(candidate_votes())
    c<-c |> arrange(desc(detected_date),candidate_id)
    tagList(lapply(seq_len(nrow(c)),function(i){
      r<-c[i,]
      cid<-as.character(r$candidate_id)
      key<-candidate_vote_key(cid)
      row<-candidate_vote_row(v0,cid)
      div(class="candidate-card",
        div(class="candidate-type",icon(ifelse(grepl("dataset",tolower(r$candidate_type %||% "")),"database","file-lines"))),
        div(class="candidate-copy",
          h4(r$title %||% "Untitled candidate"),
          p(class="candidate-meta",paste(na.omit(c(r$source,r$detected_date,r$doi_or_accession)),collapse=" · ")),
          if(!is.na(r$matched_terms)&&nzchar(r$matched_terms)) span(class="candidate-tag",r$matched_terms)
        ),
        div(id=paste0("candidate-vote-block-",key),class="candidate-vote-block",`data-pending`="0",`data-candidate-id`=cid,
          div(class="candidate-vote-status",
            span(class="candidate-balance",strong(`data-vote-role`="balance",ifelse(row$balance>0,paste0("+",row$balance),row$balance)),span(" review balance")),
            span(class="candidate-counts",span(`data-vote-role`="for",row$votes_for)," for · ",span(`data-vote-role`="against",row$votes_against)," against")
          ),
          div(class="candidate-vote-buttons",
            tags$button(type="button",class="candidate-vote candidate-vote-up candidate-vote-js",`data-candidate-id`=cid,`data-candidate-key`=key,`data-vote-direction`="for",`aria-pressed`="false",icon("thumbs-up"),span("For review")),
            tags$button(type="button",class="candidate-vote candidate-vote-down candidate-vote-js",`data-candidate-id`=cid,`data-candidate-key`=key,`data-vote-direction`="against",`aria-pressed`="false",icon("thumbs-down"),span("Against priority"))
          ),
          div(class="candidate-vote-message",`data-vote-role`="message",`aria-live`="polite")
        )
      )
    }))
  })

  # One event channel handles every candidate. Errors are caught and returned
  # to the clicked card rather than escaping the observer and destabilizing UI.
  observeEvent(input$candidate_vote_event,{
    payload<-input$candidate_vote_event
    cid<-tryCatch(as.character(payload$candidate_id %||% "")[[1]],error=function(e)"")
    key<-tryCatch(as.character(payload$candidate_key %||% candidate_vote_key(cid))[[1]],error=function(e)candidate_vote_key(cid))
    direction<-tryCatch(as.character(payload$direction %||% "")[[1]],error=function(e)"")
    ack<-tryCatch({
      res<-cast_candidate_vote(cid,direction)
      c(list(candidate_id=cid,candidate_key=key),res)
    },error=function(e){
      list(ok=FALSE,candidate_id=cid,candidate_key=key,direction=direction,message=paste0("Vote could not be recorded: ",conditionMessage(e)))
    })
    session$sendCustomMessage("candidate_vote_ack",ack)
  },ignoreInit=TRUE,priority=100)

  output$fox_candidate_table <- renderDT({
    c<-mfox$candidates |> left_join(vote_counts(),by="candidate_id") |> mutate(across(c(votes_for,votes_against,balance,total_votes),~replace_na(.x,0L))) |> arrange(desc(balance),desc(total_votes),desc(detected_date))
    c |> select(candidate_id,title,source,detected_date,candidate_type,triage_status,`For`=votes_for,`Against`=votes_against,`Balance`=balance) |>
      datatable(rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE,columnDefs=list(list(className='dt-center',targets=c(6,7,8)))))
  })
  observeEvent(input$submit_contribution,{
    path <- file.path("data","contributions.csv")
    if(file.exists(path)) contributions_live(readr::read_csv(path,show_col_types=FALSE,na=c("","NA")))
  },ignoreInit=TRUE,priority=-10)

  observeEvent(input$fox_login,{
    configured <- Sys.getenv("MFOX_FOX_PASSWORD",unset="")
    if(!nzchar(configured)){
      fox_authenticated(FALSE); fox_message("Fox authentication is not configured. Set MFOX_FOX_PASSWORD before launching the app.")
    } else if(identical(input$fox_password,configured)){
      fox_authenticated(TRUE); fox_message(NULL)
    } else { fox_authenticated(FALSE); fox_message("Password not recognized.") }
  },ignoreInit=TRUE)
  observeEvent(input$fox_logout,{fox_authenticated(FALSE); updateTextInput(session,"fox_password",value="")},ignoreInit=TRUE)

  # ---- MFOX Den state -------------------------------------------------------
  # Keep the shell static after login. Only the queue/table and small detail
  # outputs update. This avoids the whole Den becoming a Shiny recalculating
  # output (the persistent grey/shaded state seen in earlier releases).
  fox_selected <- reactiveVal(NULL)
  fox_decision_message <- reactiveVal(NULL)
  fox_email_message <- reactiveVal(NULL)
  history_tick <- reactiveVal(0L)
  email_tick <- reactiveVal(0L)

  output$fox_authenticated_client <- reactive(isTRUE(fox_authenticated()))
  outputOptions(output,"fox_authenticated_client",suspendWhenHidden=FALSE)
  output$fox_login_message <- renderUI({if(is.null(fox_message()))NULL else div(class="submission-error",fox_message())})

  pending_contribs <- reactive({
    d <- contributions_live()
    if(!nrow(d)) return(d)
    if(!"status" %in% names(d)) return(d[0,])
    d |> filter(as.character(status) %in% c("Submitted","Under review","Needs information"))
  })

  output$fox_has_selection <- reactive({
    sid <- fox_selected()
    !is.null(sid) && nzchar(sid)
  })
  outputOptions(output,"fox_has_selection",suspendWhenHidden=FALSE)

  output$fox_queue_count <- renderUI({
    n <- nrow(pending_contribs())
    span(class="fox-count-chip",paste(n,ifelse(n==1,"pending","pending")))
  })

  output$fox_queue_table <- renderDT({
    d <- pending_contribs()
    if(!nrow(d)){
      d <- tibble::tibble(`Submitted`=character(),Type=character(),Title=character(),Status=character())
    } else {
      d <- d |> transmute(
        Submitted=as.character(submitted_date),
        Type=as.character(submission_type),
        Title=ifelse(is.na(title_or_name)|title_or_name=="","Untitled",as.character(title_or_name)),
        Status=as.character(status)
      )
    }
    datatable(d,rownames=FALSE,selection="single",escape=TRUE,
      options=list(pageLength=8,lengthChange=FALSE,scrollX=TRUE,dom="tip",ordering=TRUE,autoWidth=FALSE),
      class="compact stripe hover")
  })
  fox_queue_proxy <- dataTableProxy("fox_queue_table")

  read_saved_links <- function(kind,sid){
    lp <- file.path("data",paste0("curation_",kind,"_links.csv"))
    if(!file.exists(lp)) return(character())
    x <- tryCatch(readr::read_csv(lp,show_col_types=FALSE,na=c("","NA")),error=function(e)tibble::tibble())
    if(!nrow(x) || !all(c("submission_id","value") %in% names(x))) return(character())
    unique(na.omit(as.character(x$value[as.character(x$submission_id)==sid])))
  }

  reset_review_fields <- function(sid=NULL){
    if(is.null(sid)){
      updateSelectizeInput(session,"fox_sample",selected=character(0))
      updateSelectizeInput(session,"fox_context",selected=character(0))
      updateSelectizeInput(session,"fox_assay",selected=character(0))
      updateTextAreaInput(session,"fox_notes",value="")
      return(invisible(NULL))
    }
    d <- contributions_live()
    rr <- d[as.character(d$submission_id)==sid,,drop=FALSE]
    notes <- if(nrow(rr) && "curator_notes" %in% names(rr) && !is.na(rr$curator_notes[1])) as.character(rr$curator_notes[1]) else ""
    updateSelectizeInput(session,"fox_sample",selected=read_saved_links("sample",sid))
    updateSelectizeInput(session,"fox_context",selected=read_saved_links("clinical_context",sid))
    updateSelectizeInput(session,"fox_assay",selected=read_saved_links("technology",sid))
    updateTextAreaInput(session,"fox_notes",value=notes)
  }

  observeEvent(input$fox_queue_table_rows_selected,{
    rows <- input$fox_queue_table_rows_selected
    d <- pending_contribs()
    if(length(rows)!=1 || !nrow(d) || rows[1]>nrow(d)){
      fox_selected(NULL); reset_review_fields(NULL); return()
    }
    sid <- as.character(d$submission_id[rows[1]])
    fox_selected(sid)
    fox_decision_message(NULL)
    reset_review_fields(sid)
  },ignoreInit=TRUE)

  output$fox_selected_summary <- renderUI({
    sid <- fox_selected(); req(sid)
    d <- contributions_live() |> filter(as.character(submission_id)==sid)
    req(nrow(d)==1); r<-d[1,]
    div(class="fox-source-summary",
      div(class="fox-source-top",div(span(class="fox-kicker","INCOMING SUBMISSION"),h3(ifelse(is.na(r$title_or_name)||r$title_or_name=="","Untitled",r$title_or_name))),span(class="fox-status-pill",as.character(r$status))),
      div(class="fox-source-meta",span(strong("Type: "),r$submission_type),span(strong("ID: "),r$submission_id),if(!is.na(r$identifier_or_url)&&nzchar(r$identifier_or_url)) span(strong("Identifier: "),r$identifier_or_url)),
      div(class="fox-source-text",p(strong("Source sample term: "),r$sample_term_used),p(strong("How menstrual fluid is used: "),ifelse(is.na(r$mf_use_description)||r$mf_use_description=="","Not supplied",r$mf_use_description)))
    )
  })

  output$fox_decision_status <- renderUI({
    msg <- fox_decision_message(); if(is.null(msg)) return(NULL)
    div(class=if(isTRUE(msg$ok)) "fox-inline-success" else "fox-inline-error",icon(if(isTRUE(msg$ok))"circle-check" else "triangle-exclamation"),span(msg$text))
  })

  # ---- Durable writes -----------------------------------------------------
  append_history <- function(sid,decision,samples,contexts,assays,note){
    hp<-file.path("data","curation_history.csv")
    h<-tibble::tibble(event_id=paste0("REV",format(Sys.time(),"%Y%m%d%H%M%S")),submission_id=sid,reviewed_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),reviewer="Fox curator",decision=decision,canonical_sample=paste(samples,collapse="; "),clinical_context=paste(contexts,collapse="; "),assay_or_technology=paste(assays,collapse="; "),curator_notes=note)
    if(file.exists(hp)){
      oh<-readr::read_csv(hp,show_col_types=FALSE,na=c("","NA"))
      for(nm in union(names(oh),names(h))){if(!nm%in%names(oh))oh[[nm]]<-NA_character_;if(!nm%in%names(h))h[[nm]]<-NA_character_;oh[[nm]]<-as.character(oh[[nm]]);h[[nm]]<-as.character(h[[nm]])}
      readr::write_csv(bind_rows(oh,h),hp)
    } else readr::write_csv(h,hp)
  }

  write_review_links <- function(kind,sid,values){
    vals<-unique(trimws(as.character(values %||% character())))
    vals<-vals[nzchar(vals)&vals!="Not yet classified"]
    lp<-file.path("data",paste0("curation_",kind,"_links.csv"))
    old<-if(file.exists(lp))readr::read_csv(lp,show_col_types=FALSE,na=c("","NA")) else tibble::tibble(submission_id=character(),value=character(),reviewed_at=character(),reviewer=character())
    for(nm in c("submission_id","value","reviewed_at","reviewer")){if(!nm%in%names(old))old[[nm]]<-NA_character_;old[[nm]]<-as.character(old[[nm]])}
    old<-old|>filter(submission_id!=sid)
    if(length(vals)) old<-bind_rows(old,tibble::tibble(submission_id=sid,value=vals,reviewed_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),reviewer="Fox curator"))
    readr::write_csv(old,lp)
  }

  stage_accepted_candidate <- function(rr,samples,contexts,assays,note){
    cp<-file.path("data","candidates.csv")
    identifier<-as.character(rr$identifier_or_url %||% "")
    cand<-tibble::tibble(candidate_id=paste0("COMM",format(Sys.time(),"%Y%m%d%H%M%S")),detected_date=as.character(Sys.Date()),source="Community submission",title=as.character(rr$title_or_name %||% ""),year=NA_character_,pmid=NA_character_,doi=ifelse(grepl("10\\.",identifier),identifier,NA_character_),journal=NA_character_,authors=NA_character_,abstract=NA_character_,doi_or_accession=identifier,url=ifelse(grepl("^https?://",identifier),identifier,NA_character_),matched_terms=paste(unique(na.omit(c(as.character(rr$sample_term_used),samples,contexts,assays))),collapse="; "),candidate_type=as.character(rr$submission_type),triage_status="Fox-approved — needs structured curation",reviewer="Fox curator",decision_reason=note)
    if(file.exists(cp)){
      old<-readr::read_csv(cp,show_col_types=FALSE,na=c("","NA"))
      for(nm in union(names(old),names(cand))){if(!nm%in%names(old))old[[nm]]<-NA_character_;if(!nm%in%names(cand))cand[[nm]]<-NA_character_;old[[nm]]<-as.character(old[[nm]]);cand[[nm]]<-as.character(cand[[nm]])}
      readr::write_csv(bind_rows(old,cand),cp)
    } else readr::write_csv(cand,cp)
  }

  # ---- Email outbox -------------------------------------------------------
  email_config <- function(){
    key<-Sys.getenv("MFOX_RESEND_API_KEY",unset="")
    from<-Sys.getenv("MFOX_FROM_EMAIL",unset="")
    list(key=key,from=from,configured=nzchar(key)&&nzchar(from))
  }
  opt_in_value <- function(x) tolower(trimws(as.character(x %||% ""))) %in% c("true","t","1","yes","y")

  send_resend_email <- function(to,sid,title,decision,notes=""){
    cfg<-email_config()
    if(!cfg$configured) return(list(sent=FALSE,status="Queued — email service not configured"))
    if(!requireNamespace("curl",quietly=TRUE)) return(list(sent=FALSE,status="Queued — R package 'curl' is unavailable"))
    if(!requireNamespace("jsonlite",quietly=TRUE)) return(list(sent=FALSE,status="Queued — R package 'jsonlite' is unavailable"))
    subject<-paste0("MFOX submission ",sid," — ",decision)
    body<-paste0("Thank you for contributing to MFOX-Menstrual Fluid Omics eXplorer.\n\nSubmission: ",sid,"\nResource: ",title,"\nDecision: ",decision,if(nzchar(notes))paste0("\nCurator note: ",notes)else"","\n\nThis is the decision notification you requested when submitting the resource.")
    tryCatch({
      h<-curl::new_handle()
      curl::handle_setheaders(h,"Authorization"=paste("Bearer",cfg$key),"Content-Type"="application/json")
      curl::handle_setopt(h,postfields=jsonlite::toJSON(list(from=cfg$from,to=list(to),subject=subject,text=body),auto_unbox=TRUE),connecttimeout=5,timeout=12)
      res<-curl::curl_fetch_memory("https://api.resend.com/emails",handle=h)
      if(res$status_code>=200&&res$status_code<300) list(sent=TRUE,status="Sent") else list(sent=FALSE,status=paste0("Queued — Resend returned HTTP ",res$status_code))
    },error=function(e) list(sent=FALSE,status=paste0("Queued — ",conditionMessage(e))))
  }

  send_pending_notifications <- function(){
    path<-file.path("data","contributions.csv")
    if(!file.exists(path)) return(list(sent=0L,remaining=0L,message="No contributions file found."))
    d<-readr::read_csv(path,show_col_types=FALSE,na=c("","NA"))
    for(nm in c("submitter_email","notification_status","notified_at","curator_notes","status")){if(!nm%in%names(d))d[[nm]]<-NA_character_;d[[nm]]<-as.character(d[[nm]])}
    if(!"notification_opt_in"%in%names(d))d$notification_opt_in<-FALSE
    eligible<-vapply(d$notification_opt_in,opt_in_value,logical(1)) & !is.na(d$submitter_email) & nzchar(d$submitter_email) & !d$status %in% c("Submitted","Under review") & (is.na(d$notification_status)|!d$notification_status%in%c("Sent","Not requested"))
    ids<-which(eligible); sent<-0L
    if(length(ids)) for(i in ids){
      result<-send_resend_email(d$submitter_email[i],as.character(d$submission_id[i]),as.character(d$title_or_name[i] %||% "MFOX contribution"),as.character(d$status[i]),as.character(d$curator_notes[i] %||% ""))
      d$notification_status[i]<-result$status
      if(isTRUE(result$sent)){d$notified_at[i]<-format(Sys.time(),"%Y-%m-%d %H:%M:%S");sent<-sent+1L}
    }
    readr::write_csv(d,path)
    contributions_live(d); email_tick(email_tick()+1L)
    remaining<-sum(vapply(d$notification_opt_in,opt_in_value,logical(1)) & !is.na(d$submitter_email) & nzchar(d$submitter_email) & !d$status %in% c("Submitted","Under review") & d$notification_status!="Sent",na.rm=TRUE)
    list(sent=sent,remaining=remaining,message=paste(sent,"email(s) sent;",remaining,"still queued."))
  }

  output$fox_email_status <- renderUI({
    email_tick(); cfg<-email_config(); d<-contributions_live()
    pending<-0L
    if(nrow(d) && all(c("notification_opt_in","submitter_email","notification_status","status")%in%names(d))){
      pending<-sum(vapply(d$notification_opt_in,opt_in_value,logical(1)) & !is.na(d$submitter_email) & nzchar(d$submitter_email) & !as.character(d$status)%in%c("Submitted","Under review") & as.character(d$notification_status)!="Sent",na.rm=TRUE)
    }
    div(class="fox-email-badge",span(class=if(cfg$configured)"email-dot ok"else"email-dot warn"),span(if(cfg$configured)"Email configured"else"Email not configured"),span(class="fox-email-pending",paste0(pending," queued")))
  })
  observeEvent(input$fox_send_pending_email,{
    cfg<-email_config()
    if(!cfg$configured){fox_email_message(list(ok=FALSE,text="Email is not configured. Set MFOX_RESEND_API_KEY and MFOX_FROM_EMAIL, restart the app, then use Send pending emails."));return()}
    ans<-tryCatch(send_pending_notifications(),error=function(e)list(sent=0L,remaining=NA_integer_,message=paste("Email send failed:",conditionMessage(e))))
    fox_email_message(list(ok=identical(ans$remaining,0L)||ans$sent>0,text=ans$message))
  },ignoreInit=TRUE)
  output$fox_email_message <- renderUI({
    m<-fox_email_message();if(is.null(m))return(NULL)
    div(class=if(isTRUE(m$ok))"fox-inline-success"else"fox-inline-error",span(m$text))
  })

  record_fox_decision <- function(decision){
    sid<-fox_selected()
    if(is.null(sid)||!nzchar(sid)){fox_decision_message(list(ok=FALSE,text="Select a submission first."));return()}
    samples<-isolate(input$fox_sample %||% character()); contexts<-isolate(input$fox_context %||% character()); assays<-isolate(input$fox_assay %||% character()); note<-trimws(isolate(input$fox_notes %||% ""))
    new_status<-switch(decision,"Accepted"="Accepted — structured curation pending","Needs information"="Needs information","Out of scope"="Out of scope")
    result<-tryCatch({
      path<-file.path("data","contributions.csv")
      d<-readr::read_csv(path,show_col_types=FALSE,na=c("","NA"))
      idx<-which(as.character(d$submission_id)==sid);if(length(idx)!=1)stop("Submission not found uniquely in contributions.csv.")
      for(nm in c("status","curator_notes","notification_status","submitter_email")){if(!nm%in%names(d))d[[nm]]<-NA_character_;d[[nm]]<-as.character(d[[nm]])}
      if(!"notification_opt_in"%in%names(d))d$notification_opt_in<-FALSE
      d$status[idx]<-new_status;d$curator_notes[idx]<-note
      if(opt_in_value(d$notification_opt_in[idx])&&nzchar(d$submitter_email[idx]%||%"")) d$notification_status[idx]<-"Pending notification"
      readr::write_csv(d,path)
      append_history(sid,decision,samples,contexts,assays,note)
      write_review_links("sample",sid,samples);write_review_links("clinical_context",sid,contexts);write_review_links("technology",sid,assays)
      stage_warning<-NULL
      if(decision=="Accepted") stage_warning<-tryCatch({stage_accepted_candidate(d[idx,],samples,contexts,assays,note);NULL},error=function(e)conditionMessage(e))

      # Clear selection and form before publishing the changed queue.
      DT::selectRows(fox_queue_proxy,NULL)
      fox_selected(NULL);reset_review_fields(NULL)
      contributions_live(d);history_tick(history_tick()+1L);email_tick(email_tick()+1L)
      list(ok=TRUE,text=paste0(sid," saved as ",new_status,".",if(!is.null(stage_warning))paste0(" Candidate staging warning: ",stage_warning)else""))
    },error=function(e)list(ok=FALSE,text=paste("Review could not be saved:",conditionMessage(e))))
    fox_decision_message(result)
  }

  observeEvent(input$fox_accept,{record_fox_decision("Accepted")},ignoreInit=TRUE)
  observeEvent(input$fox_needs,{record_fox_decision("Needs information")},ignoreInit=TRUE)
  observeEvent(input$fox_reject,{record_fox_decision("Out of scope")},ignoreInit=TRUE)


  # ---- Curator direct evidence entry --------------------------------------
  fox_manual_origin <- reactiveVal(NULL)
  fox_manual_message <- reactiveVal(NULL)
  manual_tick <- reactiveVal(0L)

  output$fox_manual_origin_badge <- renderUI({
    sid <- fox_manual_origin()
    if(is.null(sid) || !nzchar(sid)) return(NULL)
    d <- contributions_live() |> filter(as.character(submission_id)==sid)
    if(!nrow(d)) return(NULL)
    div(class="fox-manual-origin",
      icon("link"),
      span(strong("Curating submission "),sid," · ",as.character(d$submission_type[1])),
      actionButton("fox_manual_detach","Detach from submission",class="btn btn-sm btn-outline-secondary")
    )
  })
  observeEvent(input$fox_manual_detach,{fox_manual_origin(NULL)},ignoreInit=TRUE)

  next_curated_id <- function(prefix, ids, width=3){
    ids <- as.character(ids %||% character())
    nums <- suppressWarnings(as.integer(gsub("[^0-9]","",ids)))
    n <- if(length(nums) && any(is.finite(nums),na.rm=TRUE)) max(nums,na.rm=TRUE)+1L else 1L
    paste0(prefix,sprintf(paste0("%0",width,"d"),n))
  }

  append_aligned <- function(old,row){
    row <- tibble::as_tibble(row)
    for(nm in union(names(old),names(row))){
      if(!nm %in% names(old)) old[[nm]] <- NA_character_
      if(!nm %in% names(row)) row[[nm]] <- NA_character_
      old[[nm]] <- as.character(old[[nm]])
      row[[nm]] <- as.character(row[[nm]])
    }
    row <- row[,names(old),drop=FALSE]
    dplyr::bind_rows(old,row)
  }

  read_table_for_write <- function(name){
    readr::read_csv(file.path("data",paste0(name,".csv")),show_col_types=FALSE,na=c("","NA"))
  }

  commit_curated_tables <- function(prepared,tag="manual"){
    stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
    backup_dir <- file.path("data","curation_backups",paste0(tag,"_",stamp))
    dir.create(backup_dir,recursive=TRUE,showWarnings=FALSE)
    for(nm in names(prepared)){
      srcp <- file.path("data",paste0(nm,".csv"))
      if(file.exists(srcp)) file.copy(srcp,file.path(backup_dir,basename(srcp)),overwrite=TRUE)
    }
    tmp <- lapply(names(prepared),function(nm){
      tp <- tempfile(pattern=paste0(nm,"_"),tmpdir="data",fileext=".csv")
      readr::write_csv(prepared[[nm]],tp); tp
    }); names(tmp)<-names(prepared)
    on.exit(unlink(unlist(tmp)),add=TRUE)
    for(nm in names(prepared)){
      target <- file.path("data",paste0(nm,".csv"))
      if(!file.copy(tmp[[nm]],target,overwrite=TRUE)) stop("Could not replace ",target)
    }
    backup_dir
  }

  refresh_mfox_session <- function(){
    fresh <- load_mfox_data()
    verr <- validate_mfox(fresh)
    assign("mfox",fresh,envir=.GlobalEnv)
    assign("validation_errors",verr,envir=.GlobalEnv)
    db_tick(db_tick()+1L); manual_tick(manual_tick()+1L)
    updateSelectizeInput(session,"fox_manual_existing_study",
      choices=setNames(fresh$studies$study_id,paste0(fresh$studies$study_id," · ",fresh$studies$year," · ",fresh$studies$title)),server=TRUE)
    updateSelectizeInput(session,"fox_manual_dataset_assay_id",
      choices=setNames(fresh$assays$assay_id,paste0(fresh$assays$study_id," · ",fresh$assays$assay_id," · ",fresh$assays$omics_modality," · ",sample_label(fresh$assays$biospecimen_class))),server=TRUE)
    updateSelectizeInput(session,"fox_manual_project_studies",
      choices=setNames(fresh$studies$study_id,paste0(fresh$studies$study_id," · ",fresh$studies$year," · ",fresh$studies$title)),server=TRUE)
    updateSelectizeInput(session,"fox_manual_project_companies",
      choices=setNames(fresh$companies$company_id,paste0(fresh$companies$company_id," · ",fresh$companies$company)),server=TRUE)
    invisible(fresh)
  }

  sample_provenance <- function(sample){
    if(sample=="Whole menstrual fluid") return(list(derivation="Direct specimen",manip="None",culture="Uncultured",direct="Yes"))
    if(sample %in% c("Menstrual-fluid cells","Tissue fragments","Cells/tissue — not further separated","Cell-free menstrual fluid","Extracellular vesicles"))
      return(list(derivation="Directly derived fraction",manip="Isolation / fractionation",culture="Uncultured",direct="Yes"))
    if(sample=="Cultured menstrual-derived cells") return(list(derivation="Cultured derivative",manip="Expanded culture",culture="Cultured",direct="No"))
    list(derivation="Cultured/isolated derivative",manip="Experimental manipulation",culture="Manipulated derivative",direct="No")
  }

  reset_manual_form <- function(){
    updateTextInput(session,"fox_manual_title",value="")
    updateNumericInput(session,"fox_manual_year",value=as.integer(format(Sys.Date(),"%Y")))
    updateTextInput(session,"fox_manual_doi",value=""); updateTextInput(session,"fox_manual_pmid",value="")
    updateTextInput(session,"fox_manual_journal",value=""); updateTextInput(session,"fox_manual_first_author",value="")
    updateTextInput(session,"fox_manual_country",value=""); updateTextInput(session,"fox_manual_source_url",value="")
    updateTextInput(session,"fox_manual_n_total",value="")
    updateTextInput(session,"fox_manual_cohort_name",value="Main cohort")
    updateSelectizeInput(session,"fox_manual_context",selected=character(0))
    updateTextInput(session,"fox_manual_condition",value=""); updateTextInput(session,"fox_manual_n_participants",value="")
    updateTextInput(session,"fox_manual_menstrual_day",value="NR"); updateTextInput(session,"fox_manual_collection_device",value="NR")
    updateTextInput(session,"fox_manual_sample_fraction",value="")
    updateSelectizeInput(session,"fox_manual_assay_family",selected=character(0))
    updateSelectizeInput(session,"fox_manual_modality",selected=character(0))
    updateTextInput(session,"fox_manual_assay_type",value=""); updateTextInput(session,"fox_manual_platform",value="")
    updateSelectizeInput(session,"fox_manual_domain",selected=character(0))
    updateTextInput(session,"fox_manual_comparison",value=""); updateTextAreaInput(session,"fox_manual_notes",value="")
    updateTextInput(session,"fox_manual_repository",value=""); updateTextInput(session,"fox_manual_accession",value="")
    updateTextInput(session,"fox_manual_dataset_url",value=""); updateTextInput(session,"fox_manual_data_type",value="")
    updateSelectizeInput(session,"fox_manual_dataset_assay_id",selected=character(0))
    updateTextInput(session,"fox_manual_ds_repository",value=""); updateTextInput(session,"fox_manual_ds_accession",value="")
    updateTextInput(session,"fox_manual_ds_url",value=""); updateTextInput(session,"fox_manual_ds_type",value="")
    updateTextAreaInput(session,"fox_manual_ds_notes",value="")
    updateTextInput(session,"fox_manual_project_name",value=""); updateTextInput(session,"fox_manual_project_lead",value="")
    updateTextInput(session,"fox_manual_project_country",value=""); updateTextInput(session,"fox_manual_project_start",value="")
    updateTextInput(session,"fox_manual_project_end",value=""); updateTextAreaInput(session,"fox_manual_project_focus",value="")
    updateTextInput(session,"fox_manual_project_website",value=""); updateTextInput(session,"fox_manual_project_source",value="")
    updateSelectizeInput(session,"fox_manual_project_studies",selected=character(0)); updateSelectizeInput(session,"fox_manual_project_companies",selected=character(0))
    updateTextAreaInput(session,"fox_manual_project_notes",value="")
    updateTextInput(session,"fox_manual_company_name",value=""); updateTextInput(session,"fox_manual_company_website",value="")
    updateTextInput(session,"fox_manual_company_city",value=""); updateTextInput(session,"fox_manual_company_country",value="")
    updateTextInput(session,"fox_manual_company_collection",value=""); updateTextInput(session,"fox_manual_company_technology",value="")
    updateTextInput(session,"fox_manual_company_application",value=""); updateSelectizeInput(session,"fox_manual_company_use",selected=character(0))
    updateSelectizeInput(session,"fox_manual_company_material",selected=character(0))
    updateSelectInput(session,"fox_manual_company_maturity",selected="Early-stage feasibility")
    updateSelectInput(session,"fox_manual_company_evidence",selected="Company-reported / emerging")
    updateTextInput(session,"fox_manual_company_stage",value="")
    updateTextInput(session,"fox_manual_company_source",value=""); updateTextAreaInput(session,"fox_manual_company_note",value="")
    updateTextInput(session,"fox_manual_person_name",value=""); updateTextInput(session,"fox_manual_person_institution",value="")
    updateTextInput(session,"fox_manual_person_city",value=""); updateTextInput(session,"fox_manual_person_country",value="")
    updateTextInput(session,"fox_manual_person_role",value=""); updateTextInput(session,"fox_manual_person_orcid",value="")
    updateTextInput(session,"fox_manual_person_profile",value=""); updateTextInput(session,"fox_manual_person_expertise",value="")
    updateTextAreaInput(session,"fox_manual_person_notes",value="")
  }
  observeEvent(input$fox_manual_reset,{fox_manual_message(NULL);fox_manual_origin(NULL);reset_manual_form()},ignoreInit=TRUE)

  observeEvent(input$fox_curate_direct,{
    sid <- fox_selected()
    if(is.null(sid)||!nzchar(sid)){fox_decision_message(list(ok=FALSE,text="Select a submission first."));return()}
    d <- contributions_live() |> filter(as.character(submission_id)==sid)
    if(!nrow(d)) return()
    r <- d[1,]
    typ <- as.character(r$submission_type %||% "")
    resource <- if(grepl("Publication",typ,ignore.case=TRUE)) "evidence" else if(grepl("Dataset",typ,ignore.case=TRUE)) "dataset" else if(grepl("Project|consortium|program",typ,ignore.case=TRUE)) "project" else if(grepl("Company",typ,ignore.case=TRUE)) "company" else if(grepl("Researcher|lab",typ,ignore.case=TRUE)) "person" else NA_character_
    if(is.na(resource)){
      fox_decision_message(list(ok=FALSE,text="This submission type does not have a direct structured destination yet. Use curator notes / Needs information, or add the appropriate resource separately in Add evidence."))
      return()
    }
    reset_manual_form()
    fox_manual_origin(sid)
    updateSelectInput(session,"fox_manual_resource",selected=resource)
    ident <- trimws(as.character(r$identifier_or_url %||% ""))
    ttl <- as.character(r$title_or_name %||% "")
    if(resource=="evidence"){
      updateRadioButtons(session,"fox_manual_mode",selected="new")
      updateTextInput(session,"fox_manual_title",value=ttl)
      if(grepl("^10\\.",ident)) updateTextInput(session,"fox_manual_doi",value=ident)
      else if(grepl("^[0-9]{6,9}$",ident)) updateTextInput(session,"fox_manual_pmid",value=ident)
      else if(grepl("^https?://",ident,ignore.case=TRUE)) updateTextInput(session,"fox_manual_source_url",value=ident)
      sm <- isolate(input$fox_sample %||% character()); cx <- isolate(input$fox_context %||% character()); ax <- isolate(input$fox_assay %||% character())
      if(length(sm)) updateSelectInput(session,"fox_manual_sample",selected=sm[1])
      if(length(cx)){
        c1 <- cx[1]
        c1 <- dplyr::recode(c1,"Healthy/reference"="Healthy / reference","Other/mixed"="Other / mixed",.default=c1)
        updateSelectizeInput(session,"fox_manual_context",selected=c1)
      }
      if(length(ax)){
        if(ax[1] %in% mfox$assays$assay_family) updateSelectizeInput(session,"fox_manual_assay_family",selected=ax[1])
        if(ax[1] %in% mfox$assays$omics_modality) updateSelectizeInput(session,"fox_manual_modality",selected=ax[1])
      }
      updateTextInput(session,"fox_manual_sample_fraction",value=as.character(r$sample_term_used %||% ""))
      updateTextAreaInput(session,"fox_manual_notes",value=trimws(paste(as.character(r$mf_use_description %||% ""),as.character(r$notes %||% ""))))
    } else if(resource=="dataset"){
      if(grepl("^https?://",ident,ignore.case=TRUE)) updateTextInput(session,"fox_manual_ds_url",value=ident) else updateTextInput(session,"fox_manual_ds_accession",value=ident)
      repo <- if(grepl("^GSE|^GSM",ident,ignore.case=TRUE)) "GEO" else if(grepl("^PRJNA|^SR[ARPX]",ident,ignore.case=TRUE)) "SRA/BioProject" else if(grepl("^PXD",ident,ignore.case=TRUE)) "PRIDE/ProteomeXchange" else ""
      updateTextInput(session,"fox_manual_ds_repository",value=repo)
      updateTextAreaInput(session,"fox_manual_ds_notes",value=trimws(paste(ttl,as.character(r$mf_use_description %||% ""),as.character(r$notes %||% ""))))
    } else if(resource=="project"){
      updateTextInput(session,"fox_manual_project_name",value=ttl)
      if(grepl("^https?://",ident,ignore.case=TRUE)){
        updateTextInput(session,"fox_manual_project_website",value=ident)
        updateTextInput(session,"fox_manual_project_source",value=ident)
      }
      updateTextAreaInput(session,"fox_manual_project_focus",value=as.character(r$mf_use_description %||% ""))
      updateTextAreaInput(session,"fox_manual_project_notes",value=as.character(r$notes %||% ""))
    } else if(resource=="company"){
      updateTextInput(session,"fox_manual_company_name",value=ttl)
      if(grepl("^https?://",ident,ignore.case=TRUE)) updateTextInput(session,"fox_manual_company_website",value=ident)
      updateTextInput(session,"fox_manual_company_source",value=ifelse(grepl("^https?://",ident,ignore.case=TRUE),ident,""))
      updateTextAreaInput(session,"fox_manual_company_note",value=trimws(paste(as.character(r$mf_use_description %||% ""),as.character(r$notes %||% ""))))
    } else if(resource=="person"){
      updateTextInput(session,"fox_manual_person_name",value=ttl)
      if(grepl("^https?://",ident,ignore.case=TRUE)) updateTextInput(session,"fox_manual_person_profile",value=ident)
      updateTextInput(session,"fox_manual_person_expertise",value=as.character(r$mf_use_description %||% ""))
      updateTextAreaInput(session,"fox_manual_person_notes",value=as.character(r$notes %||% ""))
    }
    bslib::nav_select("fox_den_tabs",selected="Add evidence",session=session)
  },ignoreInit=TRUE)

  observeEvent(input$fox_manual_existing_study,{
    sid <- input$fox_manual_existing_study
    ch <- c("Create a new cohort"="__NEW__")
    if(!is.null(sid) && nzchar(sid)){
      cc <- mfox$cohorts |> filter(study_id==sid)
      if(nrow(cc)) ch <- c(ch,setNames(as.character(cc$cohort_id),paste0(cc$cohort_id," · ",cc$cohort_name)))
    }
    updateSelectInput(session,"fox_manual_existing_cohort",choices=ch,selected=unname(ch[1]))
  },ignoreInit=FALSE)


  finalize_curated_origin <- function(note="Structured curation completed in MFOX Den"){
    sid <- fox_manual_origin()
    if(is.null(sid)||!nzchar(sid)) return(invisible(NULL))
    path <- file.path("data","contributions.csv")
    d <- readr::read_csv(path,show_col_types=FALSE,na=c("","NA"))
    idx <- which(as.character(d$submission_id)==sid)
    if(length(idx)!=1) stop("Origin submission could not be found uniquely.")
    for(nm in c("status","curator_notes","notification_status","submitter_email")){
      if(!nm%in%names(d)) d[[nm]]<-NA_character_
      d[[nm]]<-as.character(d[[nm]])
    }
    if(!"notification_opt_in"%in%names(d)) d$notification_opt_in<-FALSE
    d$status[idx] <- "Accepted — curated into database"
    d$curator_notes[idx] <- trimws(paste(d$curator_notes[idx] %||% "",note))
    if(opt_in_value(d$notification_opt_in[idx])&&nzchar(d$submitter_email[idx]%||%"")) d$notification_status[idx] <- "Pending notification"
    readr::write_csv(d,path)
    append_history(sid,"Accepted + curated",read_saved_links("sample",sid),read_saved_links("clinical_context",sid),read_saved_links("technology",sid),note)
    contributions_live(d);history_tick(history_tick()+1L);email_tick(email_tick()+1L)
    try(DT::selectRows(fox_queue_proxy,NULL),silent=TRUE)
    fox_selected(NULL);reset_review_fields(NULL);fox_manual_origin(NULL)
    invisible(sid)
  }

  observeEvent(input$fox_manual_save,{
    fox_manual_message(NULL)
    resource <- input$fox_manual_resource %||% "evidence"
    origin_sid <- fox_manual_origin()

    result <- tryCatch({
      if(resource=="dataset"){
        aid <- trimws(input$fox_manual_dataset_assay_id %||% "")
        repo <- trimws(input$fox_manual_ds_repository %||% "")
        acc <- trimws(input$fox_manual_ds_accession %||% "")
        if(!nzchar(aid)) stop("Select the curated assay this dataset belongs to.")
        if(!nzchar(repo)) stop("Repository is required.")
        if(!nzchar(acc)) stop("Accession is required.")
        asy <- read_table_for_write("assays")
        hit <- asy |> filter(as.character(assay_id)==aid)
        if(nrow(hit)!=1) stop("Selected assay was not found uniquely.")
        ds <- read_table_for_write("datasets")
        if(any(tolower(trimws(as.character(ds$accession)))==tolower(acc),na.rm=TRUE)) stop("This dataset accession already exists in MFOX.")
        did <- next_curated_id("D",ds$dataset_id,3)
        dr <- tibble::tibble(dataset_id=did,study_id=as.character(hit$study_id[1]),assay_id=aid,repository=repo,accession=acc,
          data_type=trimws(input$fox_manual_ds_type %||% ""),raw_available=input$fox_manual_ds_raw,processed_available=input$fox_manual_ds_processed,
          metadata_available=input$fox_manual_ds_metadata,code_available=input$fox_manual_ds_code,dataset_url=trimws(input$fox_manual_ds_url %||% ""),
          date_checked=as.character(Sys.Date()),notes=trimws(input$fox_manual_ds_notes %||% ""))
        ds <- append_aligned(ds,dr)
        lg <- readr::read_csv(file.path("data","manual_evidence_log.csv"),show_col_types=FALSE,na=c("","NA"))
        lg <- append_aligned(lg,tibble::tibble(manual_id=paste0("MAN",format(Sys.time(),"%Y%m%d%H%M%S")),added_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          reviewer="Fox curator",action="Reusable dataset added",study_id=as.character(hit$study_id[1]),cohort_id=as.character(hit$cohort_id[1]),assay_id=aid,
          biospecimen_id=NA_character_,dataset_id=did,publication_id=NA_character_,title=acc,notes=trimws(input$fox_manual_ds_notes %||% "")))
        ul <- read_table_for_write("update_log")
        ul <- append_aligned(ul,tibble::tibble(update_id=next_curated_id("UPD",ul$update_id,4),run_date=as.character(Sys.Date()),source="MFOX Den manual curation",
          query_version="direct_resource_v1",records_found="1",new_candidates="0",duplicates="0",notes=paste0("Curated dataset ",did," attached to ",aid)))
        backup <- commit_curated_tables(list(datasets=ds,update_log=ul,manual_evidence_log=lg),"dataset")
        refresh_mfox_session()
        list(ok=TRUE,text=paste0("Dataset ",did," added to curated MFOX and linked to ",aid,". Backup: ",backup))
      } else if(resource=="project"){
        nm <- trimws(input$fox_manual_project_name %||% "")
        lead <- trimws(input$fox_manual_project_lead %||% "")
        focus <- trimws(input$fox_manual_project_focus %||% "")
        src <- trimws(input$fox_manual_project_source %||% "")
        if(!nzchar(nm)) stop("Project / program name is required.")
        if(!nzchar(lead)) stop("Lead organization is required.")
        if(!nzchar(focus)) stop("Project focus / scope is required.")
        if(!nzchar(src)) stop("A verification/source URL is required.")
        pr <- read_table_for_write("projects")
        if(any(tolower(trimws(as.character(pr$project_name)))==tolower(nm),na.rm=TRUE)) stop("A project/program with this name already exists in MFOX.")
        pid <- next_curated_id("PRJ",pr$project_id,3)
        row <- tibble::tibble(project_id=pid,project_name=nm,project_type=input$fox_manual_project_type,lead_organization=lead,
          country=trimws(input$fox_manual_project_country %||% ""),start_year=trimws(input$fox_manual_project_start %||% ""),end_year=trimws(input$fox_manual_project_end %||% ""),
          status=input$fox_manual_project_status,focus=focus,website=trimws(input$fox_manual_project_website %||% ""),source_url=src,last_verified=as.character(Sys.Date()),notes=trimws(input$fox_manual_project_notes %||% ""))
        pr <- append_aligned(pr,row)
        ps <- read_table_for_write("project_studies"); pc <- read_table_for_write("project_companies")
        sids <- input$fox_manual_project_studies %||% character(); cids <- input$fox_manual_project_companies %||% character()
        if(length(sids)){
          for(sid in sids){
            ps<-append_aligned(ps,tibble::tibble(project_study_id=next_curated_id("PS",ps$project_study_id,3),project_id=pid,study_id=sid,
              relationship="Curator-verified project/program relationship",source_url=src,date_verified=as.character(Sys.Date()),notes="Added with project in MFOX Den."))
          }
        }
        if(length(cids)){
          for(cid in cids){
            pc<-append_aligned(pc,tibble::tibble(project_company_id=next_curated_id("PC",pc$project_company_id,3),project_id=pid,company_id=cid,
              relationship="Curator-verified project/program relationship",source_url=src,date_verified=as.character(Sys.Date()),notes="Added with project in MFOX Den."))
          }
        }
        lg <- readr::read_csv(file.path("data","manual_evidence_log.csv"),show_col_types=FALSE,na=c("","NA"))
        lg <- append_aligned(lg,tibble::tibble(manual_id=paste0("MAN",format(Sys.time(),"%Y%m%d%H%M%S")),added_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          reviewer="Fox curator",action="Project / program added",study_id=ifelse(length(sids),paste(sids,collapse=";"),NA_character_),cohort_id=NA_character_,assay_id=NA_character_,biospecimen_id=NA_character_,
          dataset_id=NA_character_,publication_id=NA_character_,title=nm,notes=paste0(trimws(input$fox_manual_project_notes %||% ""),if(length(cids))paste0(" Linked companies: ",paste(cids,collapse=";")) else "")))
        ul <- read_table_for_write("update_log")
        ul <- append_aligned(ul,tibble::tibble(update_id=next_curated_id("UPD",ul$update_id,4),run_date=as.character(Sys.Date()),source="MFOX Den manual curation",
          query_version="direct_resource_v2",records_found="1",new_candidates="0",duplicates="0",notes=paste0("Curated project/program ",pid," · ",nm)))
        backup <- commit_curated_tables(list(projects=pr,project_studies=ps,project_companies=pc,update_log=ul,manual_evidence_log=lg),"project")
        refresh_mfox_session()
        list(ok=TRUE,text=paste0(nm," added to Projects & programs with ",length(sids)," study link(s) and ",length(cids)," company link(s). Backup: ",backup))
      } else if(resource=="company"){
        nm <- trimws(input$fox_manual_company_name %||% ""); web <- trimws(input$fox_manual_company_website %||% "")
        tech <- trimws(input$fox_manual_company_technology %||% "")
        if(!nzchar(nm)) stop("Company name is required.")
        if(!nzchar(web)) stop("Company website is required.")
        if(!nzchar(tech)) stop("Technology / product is required.")
        if(!length(input$fox_manual_company_use %||% character())) stop("Select at least one utilization area.")
        if(!length(input$fox_manual_company_material %||% character())) stop("Select at least one material / component exploited.")
        co <- read_table_for_write("companies")
        if(any(tolower(trimws(as.character(co$company)))==tolower(nm),na.rm=TRUE)) stop("A company with this name already exists in MFOX.")
        cid <- next_curated_id("CO",co$company_id,3)
        cr <- tibble::tibble(company_id=cid,company=nm,city=trimws(input$fox_manual_company_city %||% ""),country=trimws(input$fox_manual_company_country %||% ""),
          latitude=NA_character_,longitude=NA_character_,website=web,founded_year=NA_character_,sample_collection=trimws(input$fox_manual_company_collection %||% ""),
          sample_type=input$fox_manual_company_sample,technology=tech,disease_application=trimws(input$fox_manual_company_application %||% ""),
          development_stage=trimws(input$fox_manual_company_stage %||% ""),
          utilization_domain=paste(input$fox_manual_company_use %||% character(),collapse=";"),
          material_exploited=paste(input$fox_manual_company_material %||% character(),collapse=";"),
          maturity_level=input$fox_manual_company_maturity,evidence_status=input$fox_manual_company_evidence,
          evidence_note=trimws(input$fox_manual_company_note %||% ""),
          source_url=ifelse(nzchar(trimws(input$fox_manual_company_source %||% "")),trimws(input$fox_manual_company_source),web),last_verified=as.character(Sys.Date()))
        co <- append_aligned(co,cr)
        lg <- readr::read_csv(file.path("data","manual_evidence_log.csv"),show_col_types=FALSE,na=c("","NA"))
        lg <- append_aligned(lg,tibble::tibble(manual_id=paste0("MAN",format(Sys.time(),"%Y%m%d%H%M%S")),added_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          reviewer="Fox curator",action="Company added",study_id=NA_character_,cohort_id=NA_character_,assay_id=NA_character_,biospecimen_id=NA_character_,
          dataset_id=NA_character_,publication_id=NA_character_,title=nm,notes=trimws(input$fox_manual_company_note %||% "")))
        ul <- read_table_for_write("update_log")
        ul <- append_aligned(ul,tibble::tibble(update_id=next_curated_id("UPD",ul$update_id,4),run_date=as.character(Sys.Date()),source="MFOX Den manual curation",
          query_version="direct_resource_v1",records_found="1",new_candidates="0",duplicates="0",notes=paste0("Curated company ",cid," · ",nm)))
        backup <- commit_curated_tables(list(companies=co,update_log=ul,manual_evidence_log=lg),"company")
        refresh_mfox_session()
        list(ok=TRUE,text=paste0(nm," added to the curated Companies table. Backup: ",backup))
      } else if(resource=="person"){
        nm <- trimws(input$fox_manual_person_name %||% ""); inst <- trimws(input$fox_manual_person_institution %||% "")
        prof <- trimws(input$fox_manual_person_profile %||% "")
        if(!nzchar(nm)) stop("Researcher or lab name is required.")
        if(!nzchar(inst)) stop("Institution is required.")
        if(!nzchar(prof)) stop("A verifiable public profile URL is required.")
        ppl <- read_table_for_write("people"); pl <- read_table_for_write("people_labs")
        if(any(tolower(trimws(as.character(ppl$name)))==tolower(nm) & tolower(trimws(as.character(ppl$institution)))==tolower(inst),na.rm=TRUE))
          stop("This researcher/lab and institution combination already exists in MFOX.")
        pid <- next_curated_id("P",ppl$person_id,3)
        pr <- tibble::tibble(person_id=pid,name=nm,institution=inst,city=trimws(input$fox_manual_person_city %||% ""),region=NA_character_,
          country=trimws(input$fox_manual_person_country %||% ""),latitude=NA_character_,longitude=NA_character_,orcid=trimws(input$fox_manual_person_orcid %||% ""),
          public_profile_url=prof,notes=paste("Community-curated profile.",trimws(input$fox_manual_person_notes %||% "")))
        ppl <- append_aligned(ppl,pr)
        plr <- tibble::tibble(person_id=pid,name=nm,role=trimws(input$fox_manual_person_role %||% ""),study_id=NA_character_,institution=inst,
          city=trimws(input$fox_manual_person_city %||% ""),region=NA_character_,country=trimws(input$fox_manual_person_country %||% ""),
          latitude=NA_character_,longitude=NA_character_,expertise_tags=trimws(input$fox_manual_person_expertise %||% ""),orcid=trimws(input$fox_manual_person_orcid %||% ""),
          public_profile_url=prof,notes=paste("Community-curated profile.",trimws(input$fox_manual_person_notes %||% "")))
        pl <- append_aligned(pl,plr)
        lg <- readr::read_csv(file.path("data","manual_evidence_log.csv"),show_col_types=FALSE,na=c("","NA"))
        lg <- append_aligned(lg,tibble::tibble(manual_id=paste0("MAN",format(Sys.time(),"%Y%m%d%H%M%S")),added_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
          reviewer="Fox curator",action="Researcher / lab added",study_id=NA_character_,cohort_id=NA_character_,assay_id=NA_character_,biospecimen_id=NA_character_,
          dataset_id=NA_character_,publication_id=NA_character_,title=nm,notes=trimws(input$fox_manual_person_notes %||% "")))
        ul <- read_table_for_write("update_log")
        ul <- append_aligned(ul,tibble::tibble(update_id=next_curated_id("UPD",ul$update_id,4),run_date=as.character(Sys.Date()),source="MFOX Den manual curation",
          query_version="direct_resource_v1",records_found="1",new_candidates="0",duplicates="0",notes=paste0("Curated community profile ",pid," · ",nm)))
        backup <- commit_curated_tables(list(people=ppl,people_labs=pl,update_log=ul,manual_evidence_log=lg),"person")
        refresh_mfox_session()
        list(ok=TRUE,text=paste0(nm," added to the curated Community database. Backup: ",backup))
      } else {
        mode <- input$fox_manual_mode %||% "new"
        title <- trimws(input$fox_manual_title %||% "")
        year <- input$fox_manual_year
        sample <- input$fox_manual_sample %||% ""
        fam <- trimws(input$fox_manual_assay_family %||% "")
        modality <- trimws(input$fox_manual_modality %||% "")
        creating_cohort <- identical(mode,"new") || identical(input$fox_manual_existing_cohort,"__NEW__")
        context <- trimws(input$fox_manual_context %||% "")
        errs <- character()
        if(mode=="new" && !nzchar(title)) errs<-c(errs,"Title is required for a new study.")
        if(mode=="new" && (is.null(year)||is.na(year))) errs<-c(errs,"Year is required for a new study.")
        if(mode=="existing" && !nzchar(input$fox_manual_existing_study %||% "")) errs<-c(errs,"Select an existing study.")
        if(mode=="existing" && is.null(input$fox_manual_existing_cohort)) errs<-c(errs,"Select an existing cohort or choose Create a new cohort.")
        if(creating_cohort && !nzchar(context)) errs<-c(errs,"Clinical context is required for a new cohort.")
        if(!nzchar(sample)) errs<-c(errs,"Canonical sample is required.")
        if(!nzchar(fam)) errs<-c(errs,"Assay family is required.")
        if(!nzchar(modality)) errs<-c(errs,"Assay modality is required.")
        if(length(errs)) stop(paste(errs,collapse=" "))

        st <- read_table_for_write("studies"); co <- read_table_for_write("cohorts")
        asy <- read_table_for_write("assays"); bio <- read_table_for_write("biospecimens")
        ds <- read_table_for_write("datasets"); cp <- read_table_for_write("community_publications")
        ppl <- read_table_for_write("people"); au <- read_table_for_write("authorships")
        ul <- read_table_for_write("update_log")
        lg <- readr::read_csv(file.path("data","manual_evidence_log.csv"),show_col_types=FALSE,na=c("","NA"))

        new_study <- identical(mode,"new")
        if(new_study){
          doi_check <- tolower(trimws(input$fox_manual_doi %||% ""));pmid_check <- trimws(input$fox_manual_pmid %||% "");title_check <- tolower(trimws(title))
          if(nzchar(doi_check) && any(tolower(trimws(as.character(st$doi)))==doi_check,na.rm=TRUE)) stop("This DOI already exists in curated studies. Use the existing-study route.")
          if(nzchar(pmid_check) && any(trimws(as.character(st$pmid))==pmid_check,na.rm=TRUE)) stop("This PMID already exists in curated studies. Use the existing-study route.")
          if(nzchar(title_check) && any(tolower(trimws(as.character(st$title)))==title_check,na.rm=TRUE)) stop("A study with this exact title already exists. Use the existing-study route.")
        }
        sid <- if(new_study) next_curated_id("S",st$study_id,3) else as.character(input$fox_manual_existing_study)
        if(new_study){
          src <- trimws(input$fox_manual_source_url %||% "");doi <- trimws(input$fox_manual_doi %||% "");pmid <- trimws(input$fox_manual_pmid %||% "")
          if(!nzchar(src)&&nzchar(doi))src<-paste0("https://doi.org/",doi);if(!nzchar(src)&&nzchar(pmid))src<-paste0("https://pubmed.ncbi.nlm.nih.gov/",pmid,"/")
          cond<-trimws(input$fox_manual_condition %||% "");if(!nzchar(cond))cond<-context
          st<-append_aligned(st,tibble::tibble(study_id=sid,title=title,year=as.character(as.integer(year)),status=input$fox_manual_pub_status,doi=doi,pmid=pmid,
            journal=trimws(input$fox_manual_journal %||% ""),first_author=trimws(input$fox_manual_first_author %||% ""),country=trimws(input$fox_manual_country %||% ""),
            population_category=context,condition=cond,study_design=input$fox_manual_design,n_total=trimws(input$fox_manual_n_total %||% ""),n_mf=NA_character_,n_controls=NA_character_,
            longitudinal=ifelse(input$fox_manual_longitudinal=="Not reported","NR",input$fox_manual_longitudinal),primary_aim=trimws(input$fox_manual_notes %||% ""),
            source_url=src,curation_status="Curated",date_verified=as.character(Sys.Date()),notes="Added directly by MFOX Den curator."))
        } else title<-as.character(st$title[match(sid,as.character(st$study_id))] %||% sid)

        create_cohort<-new_study||identical(input$fox_manual_existing_cohort,"__NEW__")
        if(create_cohort){
          cid<-next_curated_id("C",co$cohort_id,3);cond<-trimws(input$fox_manual_condition %||% "");if(!nzchar(cond))cond<-context
          co<-append_aligned(co,tibble::tibble(cohort_id=cid,study_id=sid,cohort_name=trimws(input$fox_manual_cohort_name %||% "Main cohort"),condition=cond,
            n_participants=trimws(input$fox_manual_n_participants %||% ""),age_summary="NR",contraception="NR",menstrual_day=trimws(input$fox_manual_menstrual_day %||% "NR"),
            collection_device=trimws(input$fox_manual_collection_device %||% "NR"),collection_setting="NR",time_to_processing="NR",preservative="NR",fresh_frozen="NR",
            paired_blood=input$fox_manual_paired_blood,paired_endometrium=input$fox_manual_paired_endometrium,notes="Added directly by MFOX Den curator.",clinical_context=context))
        } else {cid<-as.character(input$fox_manual_existing_cohort);if(!any(as.character(co$cohort_id)==cid & as.character(co$study_id)==sid))stop("Selected cohort does not belong to the selected study.")}

        aid<-next_curated_id("A",asy$assay_id,3);bid<-next_curated_id("B",bio$biospecimen_id,3);prov<-sample_provenance(sample)
        frac<-trimws(input$fox_manual_sample_fraction %||% "");if(!nzchar(frac))frac<-sample
        asy<-append_aligned(asy,tibble::tibble(assay_id=aid,study_id=sid,cohort_id=cid,sample_fraction=frac,omics_modality=modality,assay_type=trimws(input$fox_manual_assay_type %||% ""),
          platform=trimws(input$fox_manual_platform %||% ""),features=NA_character_,granulocytes_assessed=NA_character_,neutrophils_resolved=NA_character_,
          primary_comparison=trimws(input$fox_manual_comparison %||% ""),notes=trimws(input$fox_manual_notes %||% ""),biospecimen_class=sample,derivation_class=prov$derivation,
          ex_vivo_manipulation=prov$manip,culture_status=prov$culture,passage_number="NR",direct_mf_scope=prov$direct,assay_family=fam,research_domain=trimws(input$fox_manual_domain %||% "")))
        bio<-append_aligned(bio,tibble::tibble(biospecimen_id=bid,study_id=sid,cohort_id=cid,assay_id=aid,sample_fraction=frac,biospecimen_class=sample,
          derivation_class=prov$derivation,ex_vivo_manipulation=prov$manip,culture_status=prov$culture,passage_number="NR",direct_mf_scope=prov$direct,notes=trimws(input$fox_manual_notes %||% "")))

        did<-NA_character_
        if(nzchar(trimws(input$fox_manual_accession %||% ""))||nzchar(trimws(input$fox_manual_dataset_url %||% ""))||nzchar(trimws(input$fox_manual_repository %||% ""))){
          did<-next_curated_id("D",ds$dataset_id,3)
          ds<-append_aligned(ds,tibble::tibble(dataset_id=did,study_id=sid,assay_id=aid,repository=trimws(input$fox_manual_repository %||% ""),accession=trimws(input$fox_manual_accession %||% ""),
            data_type=trimws(input$fox_manual_data_type %||% ""),raw_available=input$fox_manual_raw,processed_available=input$fox_manual_processed,metadata_available=input$fox_manual_metadata,
            code_available=input$fox_manual_code,dataset_url=trimws(input$fox_manual_dataset_url %||% ""),date_checked=as.character(Sys.Date()),notes="Added directly by MFOX Den curator."))
        }
        pubid<-NA_character_
        if(new_study){
          pubid<-next_curated_id("CP",cp$publication_id,3);sr<-st[as.character(st$study_id)==sid,,drop=FALSE][1,]
          cp<-append_aligned(cp,tibble::tibble(publication_id=pubid,mfox_study_id=sid,title=title,year=as.character(as.integer(year)),journal=trimws(input$fox_manual_journal %||% ""),
            doi=trimws(input$fox_manual_doi %||% ""),pmid=trimws(input$fox_manual_pmid %||% ""),publication_type=input$fox_manual_pub_type,mf_scope="MFOX evidence",
            source_url=as.character(sr$source_url),date_verified=as.character(Sys.Date()),notes="Added directly by MFOX Den curator."))
          fa<-trimws(input$fox_manual_first_author %||% "")
          if(nzchar(fa)){
            hit<-which(tolower(trimws(as.character(ppl$name)))==tolower(fa))
            if(length(hit))pid<-as.character(ppl$person_id[hit[1]]) else {
              pid<-next_curated_id("P",ppl$person_id,3)
              ppl<-append_aligned(ppl,tibble::tibble(person_id=pid,name=fa,institution=NA_character_,city=NA_character_,region=NA_character_,country=trimws(input$fox_manual_country %||% ""),
                latitude=NA_character_,longitude=NA_character_,orcid=NA_character_,public_profile_url=NA_character_,notes="Added from curator direct evidence entry; affiliation/profile not yet curated."))
            }
            au<-append_aligned(au,tibble::tibble(authorship_id=next_curated_id("AU",au$authorship_id,4),publication_id=pubid,person_id=pid,author_position="1",author_count=NA_character_,
              is_first_author="Yes",is_last_author="No",is_corresponding_author="No",is_equal_contribution="No",contribution_statement_source="MFOX Den direct evidence entry",notes="Only first author captured in quick curator entry."))
          }
        }
        lg<-append_aligned(lg,tibble::tibble(manual_id=paste0("MAN",format(Sys.time(),"%Y%m%d%H%M%S")),added_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),reviewer="Fox curator",
          action=ifelse(new_study,"New study / publication","Assay/sample added to existing study"),study_id=sid,cohort_id=cid,assay_id=aid,biospecimen_id=bid,dataset_id=did,
          publication_id=pubid,title=title,notes=trimws(input$fox_manual_notes %||% "")))
        ul<-append_aligned(ul,tibble::tibble(update_id=next_curated_id("UPD",ul$update_id,4),run_date=as.character(Sys.Date()),source="MFOX Den manual curation",
          query_version="direct_entry_v2",records_found="1",new_candidates="0",duplicates="0",notes=paste0(ifelse(new_study,"New curated study","Curated assay/sample added"),": ",sid," / ",aid)))
        backup<-commit_curated_tables(list(studies=st,cohorts=co,assays=asy,biospecimens=bio,datasets=ds,community_publications=cp,people=ppl,authorships=au,update_log=ul,manual_evidence_log=lg),"evidence")
        refresh_mfox_session()
        list(ok=TRUE,text=paste0("Structured evidence added to MFOX: ",sid," / ",aid,if(!is.na(did))paste0(" / ",did)else"",". Backup: ",backup))
      }
    },error=function(e) list(ok=FALSE,text=paste("Direct curation failed:",conditionMessage(e))))

    if(isTRUE(result$ok) && !is.null(origin_sid) && nzchar(origin_sid)){
      fin_err <- tryCatch({finalize_curated_origin(result$text);NULL},error=function(e)conditionMessage(e))
      if(!is.null(fin_err)) result$text <- paste0(result$text," Database write succeeded, but the source submission could not be finalized: ",fin_err)
    }
    fox_manual_message(result)
    if(isTRUE(result$ok)) reset_manual_form()
  },ignoreInit=TRUE)

  output$fox_manual_status <- renderUI({
    m<-fox_manual_message();if(is.null(m))return(NULL)
    div(class=if(isTRUE(m$ok))"fox-inline-success"else"fox-inline-error",
        icon(if(isTRUE(m$ok))"circle-check"else"triangle-exclamation"),span(m$text))
  })

  output$fox_manual_log_table <- renderDT({
    manual_tick()
    p<-file.path("data","manual_evidence_log.csv")
    d<-if(file.exists(p))readr::read_csv(p,show_col_types=FALSE,na=c("","NA"))else tibble::tibble()
    if(!nrow(d)) return(datatable(tibble::tibble(Status="No manual evidence additions yet."),rownames=FALSE,options=list(dom="t")))
    d |> arrange(desc(added_at)) |> select(added_at,action,study_id,assay_id,dataset_id,title,notes) |>
      datatable(rownames=FALSE,options=list(pageLength=8,scrollX=TRUE))
  })

  output$fox_community_profiles_table <- renderDT({
    req(fox_authenticated())
    p <- file.path("data","community_submissions.csv")
    d <- if(file.exists(p)) readr::read_csv(p,show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character()),na=c("","NA")) else tibble::tibble()
    if(!nrow(d)) return(datatable(tibble::tibble(Status="No structured community profiles submitted yet."),rownames=FALSE,options=list(dom="t")))
    keep <- intersect(c("submission_id","submitted_date","profile_type","sector","name_or_lab","institution","role","city","region","country","website","profile_url","orcid","expertise_tags","clinical_context","sample_types","technologies","company_utilization_domain","company_development_stage","company_evidence_status","verification_url","status"),names(d))
    d <- d[,keep,drop=FALSE]
    datatable(d,rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE))
  })

  output$fox_accepted_table <- renderDT({
    d<-accepted_contributions()
    if(!nrow(d)) return(datatable(tibble::tibble(Status="No accepted submissions yet."),rownames=FALSE,options=list(dom='t')))
    hp<-file.path("data","curation_history.csv")
    hist<-if(file.exists(hp))readr::read_csv(hp,show_col_types=FALSE,na=c("","NA")) else tibble::tibble()
    if(nrow(hist)&&all(c("submission_id","decision","reviewed_at")%in%names(hist))){
      ah<-hist |> filter(grepl("^Accepted",as.character(decision))) |> group_by(submission_id) |> summarise(`Accepted at`=max(as.character(reviewed_at),na.rm=TRUE),.groups="drop")
      d<-d |> left_join(ah,by="submission_id")
    } else d$`Accepted at`<-NA_character_
    d <- d |> mutate(
      Resource=mapply(accepted_resource_link,title_or_name,identifier_or_url,USE.NAMES=FALSE),
      Destination=case_when(
        grepl("curated into database",as.character(status),ignore.case=TRUE) ~ "Curated database",
        grepl("Publication",as.character(submission_type),ignore.case=TRUE) ~ "Explore → Papers · structured mapping pending",
        grepl("Dataset",as.character(submission_type),ignore.case=TRUE) ~ "Explore → Datasets · assay/study link pending",
        grepl("Project|consortium|program",as.character(submission_type),ignore.case=TRUE) ~ "Explore → Projects & programs · relationship curation pending",
        grepl("Company",as.character(submission_type),ignore.case=TRUE) ~ "Explore → Labs & companies · company curation pending",
        grepl("Researcher|lab",as.character(submission_type),ignore.case=TRUE) ~ "Community → Researchers · profile curation pending",
        TRUE ~ "Accepted resource · curation pending"
      ),
      `Curation status`=as.character(status)
    ) |>
      select(`Accepted at`,Type=submission_type,Resource,Identifier=identifier_or_url,`Curation status`,Destination)
    datatable(d,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE))
  })

  output$fox_history_table <- renderDT({
    req(fox_authenticated());history_tick();p<-file.path("data","curation_history.csv");d<-if(file.exists(p))readr::read_csv(p,show_col_types=FALSE,na=c("","NA"))else tibble::tibble();datatable(d,rownames=FALSE,options=list(pageLength=8,scrollX=TRUE))
  })

  output$dataset_table<-renderDT({db_tick();mfox$datasets|>left_join(mfox$studies|>select(study_id,title,year),by="study_id")|>left_join(mfox$assays|>select(assay_id,omics_modality,biospecimen_class),by="assay_id")|>mutate(Accession=link_or_text(accession,dataset_url),Sample=sample_label(biospecimen_class))|>select(year,title,Technology=omics_modality,Sample,repository,Accession,raw_available,processed_available,metadata_available,code_available)|>datatable(escape=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))})
  output$candidate_public_table<-renderDT({mfox$candidates|>mutate(Paper=link_or_text(title,url))|>select(candidate_id,detected_date,Paper,year,source,candidate_type,triage_status)|>datatable(escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=12,scrollX=TRUE))})
}

shinyApp(ui,server)
