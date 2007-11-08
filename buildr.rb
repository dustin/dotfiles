#!/usr/bin/env ruby -w

SPY_REPO="http://bleu.west.spy.net/~dustin/repo/"

# Obtain an artifact from a maven 1 repository.
def m1(parts, repo_url=SPY_REPO)
  group, id, type, version = parts.split /:/

  url="#{repo_url}#{group}/#{type}s/#{id}-#{version}.#{type}"
  download(artifact(parts) => url)
end

def tree_version
  tree_ver=`hg identify`
  if not $?.success?
    raise "Failed to identify tree."
  end
  tree_ver
end
