#!/usr/bin/env ruby -w

require 'rjb'

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

def gen_build_info(pkg)
  gensrc='target/generated-src/'
  genrsrc='target/generated-rsrc'
  pkg_path=pkg.gsub(/\./, '/')
  changelog=pkg_path + "/changelog.txt"
  props_path=pkg_path + "/build.properties"

  # Path of the src files
  path=gensrc + pkg_path
  mkdir_p path
  mkdir_p "#{genrsrc}/#{pkg_path}"

  # Send the props out to the file.
  f=open "#{genrsrc}/#{props_path}", "w"
  s=Rjb::import("java.lang.System")
  %w(java.vendor java.version os.name os.version).each do |prop|
    f.write "#{prop}=#{s.getProperty(prop)}\n"
  end
  f.write "build.date=#{DateTime.now.strftime '%Y/%m/%d %H:%M'}\n"
  f.close

  # Do the changelog
  f=open "#{genrsrc}/#{changelog}", "w"
  log_input = IO.popen("hg log")
  log_input.each {|line| f.write line}
  log_input.close
  f.close
  if not $?.success?
    raise "Failed to get log"
  end

  src=<<EOF
// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>

package #{pkg};

import java.net.URL;
import java.util.Properties;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

/**
 * Information regarding this spy.jar build.
 *
 * The following properties will be set at build time:
 *
 * <ul>
 *  <li>build.date - Date of this build (yyyy/MM/dd HH:mm)</li>
 *  <li>java.vendor - Java vendor who made the VM that built this jar</li>
 *  <li>java.version - Java version of the the VM that built this jar</li>
 *  <li>os.name - Name of the OS on which this jar was built</li>
 *  <li>os.version - Version of the OS on which this jar was built</li>
 * </ul>
 */
public class BuildInfo extends Properties {

	/**
	 * Get an instance of BuildInfo that describes the spy.jar build.
	 */
	public BuildInfo() throws IOException {
		this("#{props_path}");
	}

	/**
	 * Get an instance of BuildInfo that describes the build info found in
	 * the given resource.
	 */
	protected BuildInfo(String resource) throws IOException {
		super();
		// Grab the build properties
		ClassLoader cl=getClass().getClassLoader();
		InputStream is=cl.getResourceAsStream(resource);
		if(is==null) {
			throw new IOException("No resources found for " + resource);
		}
		try {
			load(is);
		} finally {
			is.close();
		}
	}

	/**
	 * Get the date of this build.
	 */
	public Date getBuildDate() {
		SimpleDateFormat sdf=new SimpleDateFormat("yyyy/MM/dd HH:mm");
		Date rv=null;

		try {
			rv=sdf.parse(getProperty("build.date"));
		} catch(ParseException pe) {
			throw new RuntimeException(
				"Invalid date from build properties file", pe);
		}
		return(rv);
	}

	/**
	 * Get a URL to a file within this classloader.
	 *
	 * @param rel the relative name (i.e. net.spy.changelog)
	 * @return the URL
	 * @throws FileNotFoundException if the file cannot be found
	 */
	public URL getFile(String rel) throws FileNotFoundException {
		ClassLoader cl=getClass().getClassLoader();
		URL u=cl.getResource(rel);
		if(u == null) {
			throw new FileNotFoundException("Can't find " + rel);
		}
		return(u);
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuilder sb=new StringBuilder(256);

		if(getProperty("build.number") != null) {
			sb.append("build ");
			sb.append(getProperty("build.number"));
		}
		sb.append(" on ");
		sb.append(getBuildDate());
		sb.append("\\nBuild platform:  java ");
		sb.append(getProperty("java.version"));
		sb.append(" from ");
		sb.append(getProperty("java.vendor"));
		sb.append(" on ");
		sb.append(getProperty("os.name"));
		sb.append(" version ");
		sb.append(getProperty("os.version"));
		if(getProperty("tree.version") != null) {
			sb.append("\\nTree version:  ");
			sb.append(getProperty("tree.version"));
		}

		return(sb.toString());
	}

	/**
	 * Print out the build properties.
	 */
	public static void main(String args[]) throws Exception {
		BuildInfo bi=new BuildInfo();
		String cl="%" + "CHANGELOG" + "%";

		System.out.println("spy.jar " + bi);

		// If there was a changelog, let it be shown.
		if(!cl.equals("#{changelog}")) {
			if(args.length > 0 && args[0].equals("-c")) {
				System.out.println(" -- Changelog:\\n");

				URL u=bi.getFile("#{changelog}");
				InputStream is=u.openStream();
				try {
					byte data[]=new byte[8192];
					int bread=0;
					do {
						bread=is.read(data);
						if(bread > 0) {
							System.out.write(data, 0, bread);
						}
					} while(bread != -1);
				} finally {
					is.close();
				}
			} else {
				System.out.println("(add -c to see the recent changelog)");
			}
		}
	}

}
EOF
  f=open path + "/BuildInfo.java", "w"
  f.write src
  f.close
end
