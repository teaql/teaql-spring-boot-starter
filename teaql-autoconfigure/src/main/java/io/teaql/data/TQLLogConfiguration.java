package io.teaql.data;

import cn.hutool.core.map.MapUtil;
import cn.hutool.core.net.URLDecoder;
import io.teaql.data.log.LogConfiguration;
import java.nio.charset.StandardCharsets;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.ResponseBody;

@Configuration
public class TQLLogConfiguration {
  @Bean
  public LogConfiguration logConfig() {
    return new LogConfiguration();
  }

  @Controller
  public static class LogController {

    @GetMapping("/logConfig/enableGlobalMarker/{name}/")
    @ResponseBody
    public Object enableGlobalMarker(@TQLContext UserContext ctx, @PathVariable("name") String name) {
      ctx.getBean(LogConfiguration.class).enableGlobalMarker(name);
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/disableGlobalMarker/{name}/")
    @ResponseBody
    public Object disableGlobalMarker(@TQLContext UserContext ctx, @PathVariable("name") String name) {
      ctx.getBean(LogConfiguration.class).disableGlobalMarker(name);
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/addDeniedUrl/{url}/")
    @ResponseBody
    public Object addDeniedUrl(@TQLContext UserContext ctx, @PathVariable("url") String url) {
      ctx.getBean(LogConfiguration.class)
          .addDeniedUrl(URLDecoder.decode(url, StandardCharsets.UTF_8));
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/removeDeniedUrl/{url}/")
    @ResponseBody
    public Object removeDeniedUrl(@TQLContext UserContext ctx, @PathVariable("url") String url) {
      ctx.getBean(LogConfiguration.class)
          .addDeniedUrl(URLDecoder.decode(url, StandardCharsets.UTF_8));
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/enableUserMarker/{names}/")
    @ResponseBody
    public Object enableUserMarker(@TQLContext UserContext ctx, @PathVariable("names") String names) {
      ctx.getBean(LogConfiguration.class).enableUserMarker(ctx, names);
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/disableUserMarker/{names}/")
    @ResponseBody
    public Object disableUserMarker(@TQLContext UserContext ctx, @PathVariable("names") String names) {
      ctx.getBean(LogConfiguration.class).disableUserMarker(ctx, names);
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/enableAll/")
    @ResponseBody
    public Object enableAll(@TQLContext UserContext ctx) {
      ctx.getBean(LogConfiguration.class).enableAll(ctx);
      return MapUtil.of("success", true);
    }

    @GetMapping("/logConfig/reset/")
    @ResponseBody
    public Object reset(@TQLContext UserContext ctx) {
      ctx.getBean(LogConfiguration.class).enableAll(ctx);
      return MapUtil.of("success", true);
    }
  }
}
