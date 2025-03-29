package io.teaql.data.log;

import java.util.Set;

import org.slf4j.Marker;

import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

public class LogFilter extends Filter<ILoggingEvent> {

    @Override
    public FilterReply decide(ILoggingEvent event) {
        LogConfiguration config = LogConfiguration.get();
        if (config == null) {
            return FilterReply.NEUTRAL;
        }

        String tracePath = event.getMDCPropertyMap().get("TRACE_PATH");
        if (!ObjectUtil.isEmpty(tracePath)) {
            for (String deniedUrl : config.deniedUrls) {
                if (tracePath.startsWith(deniedUrl)) {
                    return FilterReply.DENY;
                }
            }
        }

        Marker marker = event.getMarker();
        if (marker == null) {
            return FilterReply.NEUTRAL;
        }

        String requestMarkers = event.getMDCPropertyMap().get("TRACE_MARKERS");
        if (ObjectUtil.isNotEmpty(requestMarkers)) {
            String[] markerNames = requestMarkers.split(",");
            if (ArrayUtil.contains(markerNames, marker.getName())) {
                return FilterReply.ACCEPT;
            }
        }

        String traceUserId = event.getMDCPropertyMap().get("TRACE_USER_ID");
        Set<Marker> markers = config.enabledMarkers;
        if (ObjectUtil.isNotEmpty(traceUserId)) {
            if (config.enabledAllUsers.contains(traceUserId)) {
                return FilterReply.ACCEPT;
            }
            markers = config.userMarkers.getOrDefault(traceUserId, markers);
        }
        if (markers.contains(marker)) {
            return FilterReply.ACCEPT;
        }
        return FilterReply.NEUTRAL;
    }
}
